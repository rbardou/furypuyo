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

type 'a socket = file_descr

type addr = sockaddr

let addr addr port =
  let addr =
    try
      (gethostbyname addr).h_addr_list.(0)
    with Not_found ->
      raise (Network_error ("Udp.addr", "Host not found: "^addr))
  in
  ADDR_INET (addr, port)

let make_addr = addr

let socket () =
  let sock = Unix.socket PF_INET SOCK_DGRAM 0 in
  Unix.set_nonblock sock;
  sock

let bind sock ?addr port =
  let addr =
    match addr with
      | None -> ADDR_INET (inet_addr_any, port)
      | Some addr -> make_addr addr port
  in
  Unix.bind sock addr

let close sock =
  Unix.close sock

let maximum_packet_size = 512

let send sock addr msg =
  let buf = Marshal.to_string msg [] in
  let len = String.length buf in
  assert (len <= maximum_packet_size);
  let sent = Unix.sendto sock buf 0 len [] addr in
  assert (sent = len)

let receive_one (sock: 'a socket) =
  let len = maximum_packet_size in
  let buf = String.create len in
  try
    let real_len, addr = Unix.recvfrom sock buf 0 len [] in
    assert (real_len > 0);
    Some (addr, (Marshal.from_string buf 0: 'a))
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
