open Unix

let max_pending = 3
  (* maximum pending connection requests for listening servers *)

exception Network_error of string * string
exception Server_is_stopped
exception Connection_is_closed

module type PROTOCOL = sig
  type message
  val ordered: message -> bool
  val important: message -> bool
end

module type NET = sig
  type message
  type server
  type connection
  val listen: int -> server
  val accept: server -> connection option
  val connect: string -> int -> connection
  val ready: connection -> bool
  val send: connection -> message -> unit
  val receive: connection -> message option
  val close: connection -> unit
  val active: connection -> bool
  val stop: server -> unit
end

module Make(P: PROTOCOL): NET with type message = P.message = struct
  type message = P.message

  type server = {
    mutable s_active: bool;
    s_socket: file_descr;
  }

  type connection = {
    mutable c_active: bool;
    c_socket: file_descr;
    c_remote_addr: sockaddr;
  }

  let listen port =
    let addr = ADDR_INET (inet_addr_of_string "127.0.0.1", port) in
    let sock = socket (domain_of_sockaddr addr) SOCK_RAW 0 in
    bind sock addr;
    listen sock max_pending;
    {
      s_active = true;
      s_socket = sock;
    }

  let stop serv =
    if serv.s_active then begin
      close serv.s_socket;
      serv.s_active <- false;
    end

  (* TODO: make non-blocking *)
  let accept serv =
    if not serv.s_active then raise Server_is_stopped;
    let sock, addr = accept serv.s_socket in
    {
      c_active = true;
      c_socket = sock;
      c_remote_addr = addr;
    }

  (* TODO: make non-blocking *)
  let connect address port =
    let addr = ADDR_INET (inet_addr_of_string address, port) in
    let sock = socket (domain_of_sockaddr addr) SOCK_RAW 0 in
    connect sock addr;
    {
      c_active = true;
      c_socket = sock;
      c_remote_addr = addr;
    }

  let close connection =
    if connection.c_active then begin
      close connection.c_socket;
      connection.c_active <- false;
    end

  let active connection = connection.c_active

  (* TODO: non-blocking "connect" and "accept" *)
  let ready connection = connection.c_active

  let send connection message =
    
end
