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

type 'a frame
  (** The type of framed connections. *)

val start: ?size: int -> 'a m channel -> 'a frame
  (** Start framing on a channel.

      @param size the maximum number of messages in the frame. Bigger sizes
      mean we have to keep more information to avoid duplicates but will reduce
      latency. Default is 100. *)

val send: ?ack: (unit -> unit) -> 'a frame -> 'a -> unit
  (** Send data over a framed channel.

      @param ack function to call if an acknowledgement is received for the
      message. *)

val resend: 'a frame -> int -> 'a -> unit
  (** Resend a message.

      [resend frame id msg]: resend message [msg] with identifier [id].
      The message is instantly resent, it is not put in a buffer. *)

val receive: 'a frame -> (int * 'a) list
  (** Receive data over a framed channel.

      Also return the identifier of messages. *)

val shift: 'a frame -> int -> unit
  (** Shift sending frame.

      [shift f id]: assume that all messages before [id] have been received
      by peer, even if they have not been acknowledged.

      You may typically call this function in the acknowledgement callback
      if some messages you sent were not important. *)

val position: 'a frame -> int
  (** Get the current position of the sending frame.

      [position frame]: return the current position of the sending frame.
      All messages before this position are assumed received or unimportant. *)

val next: 'a frame -> int
  (** Return the identifier of the next message that will be sent. *)
