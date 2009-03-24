open Unix

exception Network_error of string * string
exception Server_is_stopped
exception Connection_is_closed

module type PROTOCOL = sig
  type message
  val important: message -> bool
  val order_count: int
  val ordered: message -> int option
end

module type NET = sig
  type message
  type server
  type connection
  val listen: int -> server
  val accept: ?max: int -> server -> connection list
  val connect: string -> int -> connection
  val send: connection -> message -> unit
  val receive: ?max: int -> connection -> message list
  val close: connection -> unit
  val active: connection -> bool
  val ready: connection -> bool
  val stop: server -> unit
  val remote_address: connection -> string
  val remote_port: connection -> int
  val connections: server -> connection list
end

module RBuf: sig
  type 'a t
  val make: int -> 'a t
  val arrival:
    important: bool ->
    order: (int * int) option -> (* order, num *)
    id: int ->
    buffer: 'a t ->
    message: 'a ->
    acknowledge: (int -> unit) -> (* only important are acknowledged *)
    unit
  val take: ?max: int -> 'a t -> 'a list
end = struct
  type 'a info = {
    important: bool;
    order: int;
    num: int;
    message: 'a;
  }

  type 'a t = {
    count: int;
    orders: int array;
      (* next expected non important ordered messages *)
    mutable waiting: 'a info list;
      (* important ordered messages which are younger than expected *)
    ready: 'a Queue.t;
      (* in the order it should be processed *)
  }

  let make order_count = {
    count = order_count;
    orders = Array.make order_count 0;
    waiting = [];
    ready = Queue.create ();
  }

  let one_arrival buf info =
    if info.order >= 0 && info.order < buf.count then begin
      let next = buf.orders.(info.order) in
      if next > info.num then
        None
      else if next = info.num then begin
        buf.orders.(info.order) <- next + 1;
        Some info
      end else if info.important then begin
        buf.waiting <- info :: buf.waiting;
        None
      end else begin
        buf.orders.(info.order) <- info.num + 1;
        Some info
      end
    end else
      None

  let rec filter_options acc = function
    | [] -> List.rev acc
    | None :: rem -> filter_options acc rem
    | Some x :: rem -> filter_options (x :: acc) rem

  let rec waiting_arrival acc buf =
    let waiting = buf.waiting in
    buf.waiting <- [];
    match filter_options [] (List.map (one_arrival buf) waiting) with
      | [] -> acc
      | news -> waiting_arrival (acc @ news) buf

  let ordered_arrival buf important order num message =
    let info = {
      important = important;
      order = order;
      num = num;
      message = message;
    } in
    match one_arrival buf info with
      | None ->
          []
      | Some info ->
          List.sort
            (fun a b -> if a.order = b.order then a.num - b.num else 0)
            (info :: waiting_arrival [] buf)

  let arrival ~important ~order ~id ~buffer ~message ~acknowledge =
    (* TODO: check that id was not already received *)
    if important then acknowledge id;
    match order with
      | Some (order, num) ->
          let new_ready = ordered_arrival buffer important order num message in
          let new_ready = List.map (fun i -> i.message) new_ready in
          List.iter (fun m -> Queue.add m buffer.ready) new_ready
      | None ->
          Queue.add message buffer.ready

  let rec take_aux acc buf max =
    if max <= 0 then
      List.rev acc
    else try
      take_aux (Queue.take buf.ready :: acc) buf (max - 1)
    with Queue.Empty ->
      List.rev acc

  let take ?(max = max_int) buf =
    take_aux [] buf max
end

module SBuf: sig
  type 'a t
  val send:
    important: bool ->
    order: int option ->
    message: 'a ->
    buffer: 'a t ->
    int * int (* id, order num (if any) *)
  val acknowledge: 'a t -> int -> unit
  val make: int -> 'a t
end = struct
  type 'a t = {
    count: int;
    orders: int array;
    mutable next: int;
  }

  let make count = {
    count = count;
    orders = Array.make count 0;
    next = 0;
  }

  let acknowledge buf id =
    () (* TODO *)

  let send ~important ~order ~message ~buffer =
    let id = buffer.next in
    buffer.next <- buffer.next + 1;
    let num = match order with
      | None -> 0
      | Some order ->
          if order >= 0 && order < buffer.count then begin
            let num = buffer.orders.(order) in
            buffer.orders.(order) <- num + 1;
            num
          end else 0
    in
    id, num
end

module Make(P: PROTOCOL): NET with type message = P.message = struct
  type message = P.message

  type msg =
    | Hello
    | Accept
    | Message of int * int * message (* id, order channel (if any), message *)
    | Acknowledge of int
    | Bye

  type server = {
    mutable s_active: bool;
    s_socket: file_descr;
    s_hellos: sockaddr Queue.t;
    mutable s_connections: server_connection list;
  }

  and server_connection = {
    sc_server: server;
    mutable sc_active: bool;
    sc_remote_addr: sockaddr;
    sc_send_buffer: message SBuf.t;
    sc_reception_buffer: message RBuf.t;
  }

  let add_server_connection set connection =
    connection :: set

  let remove_server_connection set connection =
    List.filter (fun c -> c != connection) set

  let find_server_connection set addr =
    List.find (fun c -> c.sc_remote_addr = addr) set

  type client_connection = {
    mutable cc_ready: bool;
    mutable cc_active: bool;
    cc_socket: file_descr;
    cc_remote_addr: sockaddr;
    cc_send_buffer: message SBuf.t;
    cc_reception_buffer: message RBuf.t;
  }

  type connection =
    | Client of client_connection
    | Server of server_connection

  let listen port =
    let addr = ADDR_INET (inet_addr_of_string "127.0.0.1", port) in
    let sock = socket (domain_of_sockaddr addr) SOCK_DGRAM 0 in
    set_nonblock sock;
    bind sock addr;
    {
      s_active = true;
      s_socket = sock;
      s_hellos = Queue.create ();
      s_connections = [];
    }

  let maximum_packet_size = 512

  let send_msg socket remote_addr (msg: msg) =
    let buf = Marshal.to_string msg [] in
    let len = String.length buf in
    assert (len <= maximum_packet_size);
    let sent =
      sendto socket buf 0 len [] remote_addr in
    assert (sent = len)

  let receive_msg socket =
    let len = maximum_packet_size in
    let buf = String.create len in
    try
      let real_len, addr = recvfrom socket buf 0 len [] in
      assert (real_len > 0);
      Some ((Marshal.from_string buf 0: msg), addr)
    with
      | Unix_error ((EAGAIN | EWOULDBLOCK), _, _) ->
          None

  let handle_server_msg server addr = function
    | Hello ->
        Queue.add addr server.s_hellos
    | Bye ->
        begin try
          let connection = find_server_connection server.s_connections addr in
          connection.sc_active <- false;
          server.s_connections <-
            remove_server_connection server.s_connections connection
        with Not_found ->
          ()
        end
    | Message (id, num, message) ->
        begin try
          let connection = find_server_connection server.s_connections addr in
          let order =
            match P.ordered message with
              | None -> None
              | Some order -> Some (order, num)
          in
          let acknowledge id = send_msg server.s_socket addr (Acknowledge id) in
          RBuf.arrival
            ~important: (P.important message)
            ~order
            ~id
            ~buffer: connection.sc_reception_buffer
            ~message
            ~acknowledge
        with Not_found ->
          ()
        end
    | Acknowledge id ->
        begin try
          let connection = find_server_connection server.s_connections addr in
          SBuf.acknowledge connection.sc_send_buffer id
        with Not_found ->
          ()
        end
    | _ ->
        ()

  let handle_client_msg client = function
    | Accept ->
        client.cc_ready <- true
    | Bye ->
        client.cc_active <- false
    | Message (id, num, message) ->
        let order =
          match P.ordered message with
            | None -> None
            | Some order -> Some (order, num)
        in
        let acknowledge id =
          send_msg client.cc_socket client.cc_remote_addr (Acknowledge id) in
        RBuf.arrival
          ~important: (P.important message)
          ~order
          ~id
          ~buffer: client.cc_reception_buffer
          ~message
          ~acknowledge
    | Acknowledge id ->
        SBuf.acknowledge client.cc_send_buffer id
    | _ ->
        ()

  let update_server server =
    try
      while true do
        match receive_msg server.s_socket with
          | Some (msg, addr) ->
              handle_server_msg server addr msg
          | None ->
              raise Exit
      done
    with Exit ->
      ()

  let update_client client =
    try
      while true do
        match receive_msg client.cc_socket with
          | Some (msg, _) ->
              handle_client_msg client msg
          | None ->
              raise Exit
      done
    with Exit ->
      ()

  let update_connection = function
    | Client c -> update_client c
    | Server c -> update_server c.sc_server

  let accept server addr =
    send_msg server.s_socket addr Accept;
    let connection = {
      sc_server = server;
      sc_active = true;
      sc_remote_addr = addr;
      sc_send_buffer = SBuf.make P.order_count;
      sc_reception_buffer = RBuf.make P.order_count;
    } in
    server.s_connections <-
      add_server_connection server.s_connections connection;
    connection

  let rec accept_aux acc max server =
    if max <= 0 || Queue.is_empty server.s_hellos then
      List.rev acc
    else
      let addr = Queue.take server.s_hellos in
      let sc = accept server addr in
      accept_aux (Server sc :: acc) (max - 1) server

  let accept ?(max = max_int) server =
    update_server server;
    if not server.s_active then raise Server_is_stopped;
    accept_aux [] max server

  let connect address port =
    let addr = ADDR_INET (inet_addr_of_string address, port) in
    let sock = socket (domain_of_sockaddr addr) SOCK_DGRAM 0 in
    set_nonblock sock;
    connect sock addr;
    send_msg sock addr Hello;
    let connection = {
      cc_ready = false;
      cc_active = true;
      cc_socket = sock;
      cc_remote_addr = addr;
      cc_send_buffer = SBuf.make P.order_count;
      cc_reception_buffer = RBuf.make P.order_count;
    } in
    Client connection

  let stop server =
    if server.s_active then begin
      close server.s_socket;
      server.s_active <- false;
      List.iter
        (fun sc ->
           send_msg server.s_socket sc.sc_remote_addr Bye;
           sc.sc_active <- false)
        server.s_connections;
      server.s_connections <- [];
    end

  let close = function
    | Client c ->
        if c.cc_active then begin
          send_msg c.cc_socket c.cc_remote_addr Bye;
          close c.cc_socket;
          c.cc_active <- false;
        end
    | Server c ->
        if c.sc_active then begin
          send_msg c.sc_server.s_socket c.sc_remote_addr Bye;
          c.sc_active <- false;
          c.sc_server.s_connections <-
            remove_server_connection c.sc_server.s_connections c
        end

  let active c =
    update_connection c;
    match c with
      | Client c -> c.cc_active
      | Server c -> c.sc_active

  let ready c =
    update_connection c;
    match c with
      | Client c -> c.cc_ready
      | Server c -> true

  let sbuf = function
    | Client c -> c.cc_send_buffer
    | Server c -> c.sc_send_buffer

  let send_msg_c c msg =
    match c with
      | Client c -> send_msg c.cc_socket c.cc_remote_addr msg
      | Server c -> send_msg c.sc_server.s_socket c.sc_remote_addr msg

  let send connection message =
    if not (active connection) then raise Connection_is_closed;
    update_connection connection;
    let id, num = SBuf.send
      ~important: (P.important message)
      ~order: (P.ordered message)
      ~message
      ~buffer: (sbuf connection)
    in
    send_msg_c connection (Message (id, num, message))

  let rbuf = function
    | Client c -> c.cc_reception_buffer
    | Server c -> c.sc_reception_buffer

  let receive ?max connection =
    if not (active connection) then raise Connection_is_closed;
    update_connection connection;
    RBuf.take ?max (rbuf connection)

  let get_address = function
    | ADDR_UNIX _ ->
        assert false
    | ADDR_INET (addr, _) ->
        string_of_inet_addr addr

  let get_port = function
    | ADDR_UNIX _ ->
        assert false
    | ADDR_INET (_, port) ->
        port

  let remote_address = function
    | Client c -> get_address c.cc_remote_addr
    | Server c -> get_address c.sc_remote_addr

  let remote_port = function
    | Client c -> get_port c.cc_remote_addr
    | Server c -> get_port c.sc_remote_addr

  let connections server =
    List.map (fun x -> Server x) server.s_connections
end
