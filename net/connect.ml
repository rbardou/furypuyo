(**************************************************************************)
(* Copyright (c) 2009, Romain BARDOU                                      *)
(* All rights reserved.                                                   *)
(*                                                                        *)
(* Redistribution and  use in  source and binary  forms, with  or without *)
(* modification, are permitted provided that the following conditions are *)
(* met:                                                                   *)
(*                                                                        *)
(* * Redistributions  of  source code  must  retain  the above  copyright *)
(*   notice, this list of conditions and the following disclaimer.        *)
(* * Redistributions in  binary form  must reproduce the  above copyright *)
(*   notice, this list of conditions  and the following disclaimer in the *)
(*   documentation and/or other materials provided with the distribution. *)
(* * Neither the name of Fury Puyo nor  the names of its contributors may *)
(*   be used  to endorse or  promote products derived  from this software *)
(*   without specific prior written permission.                           *)
(*                                                                        *)
(* THIS SOFTWARE  IS PROVIDED BY  THE COPYRIGHT HOLDERS  AND CONTRIBUTORS *)
(* "AS  IS" AND  ANY EXPRESS  OR IMPLIED  WARRANTIES, INCLUDING,  BUT NOT *)
(* LIMITED TO, THE IMPLIED  WARRANTIES OF MERCHANTABILITY AND FITNESS FOR *)
(* A PARTICULAR PURPOSE  ARE DISCLAIMED. IN NO EVENT  SHALL THE COPYRIGHT *)
(* OWNER OR CONTRIBUTORS BE  LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, *)
(* SPECIAL,  EXEMPLARY,  OR  CONSEQUENTIAL  DAMAGES (INCLUDING,  BUT  NOT *)
(* LIMITED TO, PROCUREMENT OF SUBSTITUTE  GOODS OR SERVICES; LOSS OF USE, *)
(* DATA, OR PROFITS; OR BUSINESS  INTERRUPTION) HOWEVER CAUSED AND ON ANY *)
(* THEORY OF  LIABILITY, WHETHER IN  CONTRACT, STRICT LIABILITY,  OR TORT *)
(* (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING  IN ANY WAY OUT OF THE USE *)
(* OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.   *)
(**************************************************************************)

let initial_resend_delay = 10
let resend_delay_rate = 2.

open Udp

type 'a msg =
  | Hello
  | Accept
  | Message of 'a
  | Bye

type 'a server = {
  mutable s_active: bool;
  s_sockets: 'a msg socket list;
  s_hellos: ('a msg socket * addr) Queue.t;
  mutable s_connections: 'a server_connection list;
}

and 'a server_connection = {
  sc_server: 'a server;
  sc_socket: 'a msg socket;
  mutable sc_active: bool;
  sc_remote_addr: addr;
  mutable sc_buffer: 'a list;
}

let add_server_connection set connection =
  connection :: set

let remove_server_connection set connection =
  List.filter (fun c -> c != connection) set

let find_server_connection set addr =
  List.find (fun c -> c.sc_remote_addr = addr) set

type 'a client_connection = {
  mutable cc_hello_last: Time.t;
  mutable cc_hello_delay: Time.d;
  mutable cc_ready: bool;
  mutable cc_active: bool;
  cc_socket: 'a msg socket;
  cc_remote_addr: addr;
  mutable cc_buffer: 'a list;
}

type 'a connection =
  | Client of 'a client_connection
  | Server of 'a server_connection

let send_sc c = Udp.send c.sc_socket c.sc_remote_addr
let send_cc c = Udp.send c.cc_socket c.cc_remote_addr
let send_c = function
  | Server sc -> send_sc sc
  | Client cc -> send_cc cc

let encode codec buf = function
  | Hello -> Bin.write buf Bin.int 0
  | Accept -> Bin.write buf Bin.int 1
  | Message m -> Bin.write buf Bin.int 2; Bin.write buf codec m
  | Bye -> Bin.write buf Bin.int 3

let decode codec buf =
  match Bin.read buf Bin.int with
    | 0 -> Hello
    | 1 -> Accept
    | 2 -> Message (Bin.read buf codec)
    | 3 -> Bye
    | _ -> failwith "Connect.decode"

let convert_codec codec =
  Bin.custom (encode codec) (decode codec)

let server_bind ?addr port codec =
  let sock = socket (convert_codec codec) in
  bind sock ?addr port;
  sock

let listen ?(addr = []) port codec =
  let sockets = match addr with
    | [] -> [ server_bind port codec ]
    | _ -> List.map (fun addr -> server_bind ~addr port codec) addr
  in
  {
    s_active = true;
    s_sockets = sockets;
    s_hellos = Queue.create ();
    s_connections = [];
  }

let accept server socket addr =
  send socket addr Accept;
  let connection = {
    sc_server = server;
    sc_socket = socket;
    sc_active = true;
    sc_remote_addr = addr;
    sc_buffer = [];
  } in
  server.s_connections <-
    add_server_connection server.s_connections connection;
  connection

let rec accept_aux acc max server =
  if max <= 0 || Queue.is_empty server.s_hellos then
    List.rev acc
  else
    let socket, addr = Queue.take server.s_hellos in
    let sc = accept server socket addr in
    accept_aux (Server sc :: acc) (max - 1) server

let handle_server_msg server socket addr = function
  | Hello ->
      begin try
        let connection = find_server_connection server.s_connections addr in
        send_sc connection Accept
      with Not_found ->
        try
          Queue.iter
            (fun (_, a) -> if a = addr then raise Exit)
            server.s_hellos;
          Queue.add (socket, addr) server.s_hellos
        with Exit ->
          ()
      end
  | Bye ->
      begin try
        let connection = find_server_connection server.s_connections addr in
        connection.sc_active <- false;
        server.s_connections <-
          remove_server_connection server.s_connections connection
      with Not_found ->
        ()
      end
  | Message msg ->
      begin try
        let connection = find_server_connection server.s_connections addr in
        connection.sc_buffer <- msg :: connection.sc_buffer;
      with Not_found ->
        ()
      end
  | Accept ->
      ()

let update_server_on_socket server socket =
  List.iter
    (fun (addr, msg) -> handle_server_msg server socket addr msg)
    (receive socket)

let update_server server =
  List.iter (update_server_on_socket server) server.s_sockets

let resend_hello client =
  let now = Time.now () in
  let shall_resend_hello =
    not client.cc_ready
    && now >= Time.shift client.cc_hello_last client.cc_hello_delay
  in
  if shall_resend_hello then begin
    client.cc_hello_last <- now;
    client.cc_hello_delay <- Time.multf client.cc_hello_delay resend_delay_rate;
    send_cc client Hello;
  end

let handle_client_msg client = function
  | Accept ->
      client.cc_ready <- true
  | Bye ->
      client.cc_active <- false
  | Message msg ->
      client.cc_buffer <- msg :: client.cc_buffer;
  | Hello ->
      ()

let update_client client =
  List.iter
    (fun (_, msg) -> handle_client_msg client msg)
    (receive client.cc_socket);
  resend_hello client

let update_connection = function
  | Client c -> update_client c
  | Server c -> update_server c.sc_server

let accept ?(max = max_int) server =
  update_server server;
  if not server.s_active then
    raise (Udp.Network_error ("Connect.accept", "Server is stopped"));
  accept_aux [] max server

let stop server =
  if server.s_active then begin
    List.iter
      (fun sc ->
         send_sc sc Bye;
         sc.sc_active <- false)
      server.s_connections;
    server.s_connections <- [];
    List.iter close server.s_sockets;
    server.s_active <- false;
  end

let close = function
  | Client c ->
      if c.cc_active then begin
        send_cc c Bye;
        close c.cc_socket;
        c.cc_active <- false;
      end
  | Server c ->
      if c.sc_active then begin
        send_sc c Bye;
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

let connect addr port codec =
  let sock = socket (convert_codec codec) in
  let client = {
    cc_hello_last = Time.now ();
    cc_hello_delay = Time.ms initial_resend_delay;
    cc_ready = false;
    cc_active = true;
    cc_socket = sock;
    cc_remote_addr = Udp.addr addr port;
    cc_buffer = [];
  } in
  send_cc client Hello;
  update_client client;
  Client client

let send connection message =
  if active connection then begin
    update_connection connection;
    send_c connection (Message message)
  end

let get_buf = function
  | Client cc -> cc.cc_buffer
  | Server sc -> sc.sc_buffer

let set_buf c v =
  match c with
    | Client cc -> cc.cc_buffer <- v
    | Server sc -> sc.sc_buffer <- v

let receive c =
  update_connection c;
  let r = get_buf c in
  set_buf c [];
  List.rev r

let receive_filter f c =
  update_connection c;
  let ok, ko = List.partition f (get_buf c) in
  set_buf c ko;
  List.rev ok

let remote_address = function
  | Client c -> Udp.string_of_addr c.cc_remote_addr
  | Server c -> Udp.string_of_addr c.sc_remote_addr

let remote_port = function
  | Client c -> Udp.port_of_addr c.cc_remote_addr
  | Server c -> Udp.port_of_addr c.sc_remote_addr
