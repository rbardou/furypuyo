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

open Unix

exception Network_error of string * string

type ('a, 'b) socket = {
  id: int;
    (* this identifier is associated to the file descriptor below in the
       file descriptor hash table *)
  fd: file_descr;
  codec_send: 'a Bin.t;
  codec_receive: 'b Bin.t;
}

type addr = sockaddr

let file_descriptors = Hashtbl.create 17

let addr addr port =
  let addr =
    try
      (gethostbyname addr).h_addr_list.(0)
    with Not_found ->
      raise (Network_error ("Udp.addr", "Host not found: "^addr))
  in
  ADDR_INET (addr, port)

let make_addr = addr

let fresh = let c = ref (-1) in fun () -> incr c; !c

let socket codec_send codec_receive =
  let sock = Unix.socket PF_INET SOCK_DGRAM 0 in
  Unix.set_nonblock sock;
  let socket = {
    id = fresh ();
    fd = sock;
    codec_send = codec_send;
    codec_receive = codec_receive;
  } in
  Hashtbl.add file_descriptors socket.id socket.fd;
  socket

let bind sock ?addr port =
  let addr =
    match addr with
      | None -> ADDR_INET (inet_addr_any, port)
      | Some addr -> make_addr addr port
  in
  Unix.bind sock.fd addr

let close sock =
  begin try Hashtbl.remove file_descriptors sock.id with Not_found -> () end;
  Unix.close sock.fd

let maximum_packet_size = 512

let send sock addr msg =
  let buf = Buffer.create maximum_packet_size in
  Bin.write (Bin.to_buffer buf) sock.codec_send msg;
  let buf = Buffer.contents buf in
  let len = String.length buf in
  assert (len <= maximum_packet_size);
  let sent = Unix.sendto sock.fd buf 0 len [] addr in
  assert (sent = len)

let receive_one sock =
  let len = maximum_packet_size in
  let buf = String.create len in
  try
    let real_len, addr = Unix.recvfrom sock.fd buf 0 len [] in
    assert (real_len > 0);
    Some (addr, (Bin.read (Bin.from_string buf) sock.codec_receive))
  with
    | Unix_error ((EAGAIN | EWOULDBLOCK), _, _) ->
        None
    | Unix_error (ECONNREFUSED, _, _) ->
        raise (Network_error ("Udp.receive", "Connection refused"))

let rec receive acc sock =
  match receive_one sock with
    | None -> List.rev acc
    | Some x -> receive (x :: acc) sock

let receive x = receive [] x

let string_of_addr = function
  | ADDR_UNIX _ ->
      assert false
  | ADDR_INET (addr, _) ->
      string_of_inet_addr addr

let port_of_addr = function
  | ADDR_UNIX _ ->
      assert false
  | ADDR_INET (_, port) ->
      port

let wait_for_input ?(timeout = -1.) () =
  let fds =
    Hashtbl.fold
      (fun _ fd acc -> fd :: acc)
      file_descriptors
      []
  in
  ignore (select fds [] [] timeout)
