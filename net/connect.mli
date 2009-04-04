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

(** Client / server connections *)

type 'a connection
  (** The type of connections. *)

type 'a server
  (** The type of servers. *)

val listen: ?addr: string list -> int -> 'a server
  (** Make a new server.

      [listen port]: make a server listening on [port] on addresses [addr].
      If [addr] is omitted, listen to all addresses. *)

val accept: ?max: int -> 'a server -> 'a connection list
  (** Accept pending connections.

      [accept serv]: accept incoming connections on server [serv]. At most
      [max] connections are returned (default is infinite). *)

val connect: string -> int -> 'a connection
  (** Connect to a server.

      [connect addr port]: connect to server at address [addr]
      on port [port]. *)

val close: 'a connection -> unit
  (** Close a connection.

      Packets that have not been received yet are lost. *)

val stop: 'a server -> unit
  (** Stop a server. *)

val ready: 'a connection -> bool
  (** Test if a connection is ready.

      Connections returned by [accept] are already ready. Connections returned
      by [connect] are not ready until the server accepts the connection. *)

val active: 'a connection -> bool
  (** Test if a connection has been closed.

      Return [false] if one of the peers have closed the connection. *)

val send: 'a connection -> 'a -> unit
  (** Send data over a connection. *)

val receive: 'a connection -> 'a list
  (** Receive data over a connection. *)

val receive_filter: ('a -> bool) -> 'a connection -> 'a list
  (** Receive data matching a predicate.

      [receive p c]: same as [receive c] but only receive data matching
      predicate [p]. Other messages are not discarded, they can still
      be received. *)

val remote_address: 'a connection -> string
  (** Get the remote address of a connection. *)

val remote_port: 'a connection -> int
  (** Get the remote port of a connection. *)
