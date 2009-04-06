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

(** Duplicates, flux and acknowledgement control *)

(** Allows to send and receive important messages, or unimportant ones if you
    call the [shift] function. Messages are not necessarily received in the
    same order they were sent, but they are identified by a unique incrementing
    integer. Messages are not resent. *)

(** First message have identifier [0], next message have identifier [1],
    next have identifier [2], and so on. *)

open Channel

type 'a m
  (** The type of messages transmitted by a frame on a channel. *)

type ('a, 'b) frame
  (** The type of framed connections. *)

val start: ?size: int -> ('a m, 'b m) channel -> ('a, 'b) frame
  (** Start framing on a channel.

      @param size the maximum number of messages in the frame. Bigger sizes
      mean we have to keep more information to avoid duplicates but will reduce
      latency. Default is 100. *)

val send: ?ack: (unit -> unit) -> ('a, 'b) frame -> 'a -> unit
  (** Send data over a framed channel.

      @param ack function to call if an acknowledgement is received for the
      message. *)

val resend: ('a, 'b) frame -> int -> 'a -> unit
  (** Resend a message.

      [resend frame id msg]: resend message [msg] with identifier [id].
      The message is instantly resent, it is not put in a buffer. *)

val receive: ('a, 'b) frame -> (int * 'b) list
  (** Receive data over a framed channel.

      Also return the identifier of messages. *)

val shift: ('a, 'b) frame -> int -> unit
  (** Shift sending frame.

      [shift f id]: assume that all messages (stricly) before [id] have been
      received by peer, even if they have not been acknowledged.

      You may typically call this function in the acknowledgement callback
      if some messages you sent were not important. *)

val position: ('a, 'b) frame -> int
  (** Get the current position of the sending frame.

      [position frame]: return the current position of the sending frame.
      All messages before this position are assumed received or unimportant. *)

val next: ('a, 'b) frame -> int
  (** Return the identifier of the next message that will be sent. *)

val size: ('a, 'b) frame -> int
  (** Return the size of the frame.

      This is the [size] parameter given to [start]. *)

val codec: 'a Bin.t -> 'a m Bin.t
