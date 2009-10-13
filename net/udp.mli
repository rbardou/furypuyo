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

(** UDP sockets *)

(** Abstraction of UNIX UDP sockets. Data are automatically marshaled.
    Addresses may be IP or DNS addresses. *)

exception Network_error of string * string
  (** Parameters are function name and error explaination. *)

type ('a, 'b) socket
  (** The type of UDP sockets.

      An [('a, 'b)] socket sends packets of type ['a] and receives packets of
      type ['b]. *)

type addr
  (** The type of internet addresses. *)

val addr: string -> int -> addr
  (** Make an internet address.

      [addr a p] is the internet address [a], which may be an IP, IPv6 or DNS
      address, on port [p]. *)

val socket: 'a Bin.t -> 'b Bin.t -> ('a, 'b) socket
  (** Make a new UDP socket. *)

val bind: ('a, 'b) socket -> ?addr: string -> int -> unit
  (** Bind an UDP socket to a local address and port.

      [bind_local socket port]: bind [socket] to [port].
      Once a socket is bound to a port, it can receive data sent to this port
      on address [addr]. If [addr] is not specified, the socket is bound to
      all addresses (LAN, Internet, ...).

      If you do not [bind] a socket, it is bound to a random port chosen by
      the system. *)

val send: ('a, 'b) socket -> addr -> 'a -> unit
  (** Send an UDP packet. *)

val receive: ('a, 'b) socket -> (addr * 'b) list
  (** Receive UDP packets. *)

val close: ('a, 'b) socket -> unit
  (** Close an UDP socket.

      The socket is no longer usable after this. *)

val string_of_addr: addr -> string
  (** Get the string part of an address. *)

val port_of_addr: addr -> int
  (** Get the port of an address. *)

val wait_for_input: ?timeout: float -> unit -> unit
  (** Wait until some input is readable on any open socket.

      Stop waiting after [timeout] seconds have passed. A negative value
      means no timeout (default behavior).

      Waiting is done using [Unix.select]. *)
