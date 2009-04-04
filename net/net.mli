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

(** Simple abstraction of network protocols *)

type channel_kind =
  | Fast (** unimportant, unordered messages *)
  | FastOrdered (** unimportant, ordered messages *)
  | Important (** important, unordered messages *)
  | Ordered (** important, ordered messages *)
  (** The type of channels kinds.
      All channel kinds prevent duplicatas and control the flow.

      Important messages are ensured to reach their destination.

      Ordered messages are ensured to be received in the same order they were
      sent. 

      The [Ordered] kind is closest to TCP, whereas [Fast] is closest to UDP
      with connections. *)

(** Protocol definition *)
module type PROTOCOL = sig
  type message
    (** The type of messages. *)

  val channel: message -> int
    (** Get the channel of a message. *)

  val channels: (int * channel_kind) list
    (** List of all channels used by the protocol. *)
end

(** Simple protocol definition *)
module type SIMPLEPROTOCOL = sig
  type message
    (** The type of messages. *)

  val kind: channel_kind
    (** Channel kind of the protocol.*)
end

(** Protocol implementation *)
module type NET = sig
  type message
    (** The type of messages sent and received in the protocol. *)

  type connection
    (** The type of connections. *)

  type server
    (** The type of servers. *)

  val listen: ?addr: string list -> int -> server
    (** Make a new server.

        [listen port]: make a server listening on [port] on addresses [addr].
        If [addr] is omitted, listen to all addresses. *)

  val accept: ?max: int -> server -> connection list
    (** Accept pending connections.

        [accept serv]: accept incoming connections on server [serv]. At most
        [max] connections are returned (default is infinite). *)

  val connect: string -> int -> connection
    (** Connect to a server.

        [connect addr port]: connect to server at address [addr]
        on port [port]. *)

  val close: connection -> unit
    (** Close a connection.

        Packets that have not been received yet are lost. *)

  val stop: server -> unit
    (** Stop a server. *)

  val ready: connection -> bool
    (** Test if a connection is ready.

        Connections returned by [accept] are already ready. Connections returned
        by [connect] are not ready until the server accepts the connection. *)

  val active: connection -> bool
    (** Test if a connection has been closed.

        Return [false] if one of the peers have closed the connection. *)

  val send: connection -> message -> unit
    (** Send data over a connection. *)

  val receive: connection -> message list
    (** Receive data over a connection. *)

  val remote_address: connection -> string
    (** Get the remote address of a connection. *)

  val remote_port: connection -> int
    (** Get the remote port of a connection. *)
end

(** Make a protocol *)
module Make(P: PROTOCOL): NET with type message = P.message

(** Make a protocol definition from a simple definition *)
module SimpleDef(P: SIMPLEPROTOCOL): PROTOCOL with type message = P.message

(** Make a simple protocol *)
module Simple(P: SIMPLEPROTOCOL): NET with type message = P.message
