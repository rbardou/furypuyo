(** Duplicates, flux and acknowledgement control *)

(** Allows to send and receive important messages, or unimportant ones if you
    call the [shift] function. Messages are not necessarily received in the
    same order they were sent, but they are identified by a unique incrementing
    integer. *)

open Channel

type 'a m
  (** The type of messages transmitted by a frame on a channel. *)

type 'a frame
  (** The type of framed connections. *)

val start: ?size: int -> 'a m channel -> 'a frame
  (** Start framing on a channel.

      @param size the maximum number of messages in the frame. Bigger sizes
      mean we have to keep more information to avoid duplicates but will reduce
      latency. *)

val send: ?ack: (unit -> unit) -> 'a frame -> 'a -> unit
  (** Send data over a framed channel.

      Return the identifier of the message.
      First message have identifier [0], next message have identifier [1],
      next have identifier [2], and so on.

      @param ack function to call if an acknowledgement is received for the
      message. *)

val send_id: ?ack: (unit -> unit) -> 'a frame -> 'a -> int
  (** Send data and get packet identifier.

      Same as [send] but return the identifier of the message.
      First message have identifier [0], next message have identifier [1],
      next have identifier [2], and so on. *)

val send_as: 'a frame -> 'a -> int -> unit
  (** Send data using a forced identifier.

      Useful to resend a packet. *)

val receive: 'a frame -> 'a list
  (** Receive data over a framed channel. *)

val receive_id: 'a frame -> (int * 'a) list
  (** Receive data and packet identifiers.

      Same as [receive] but return message identifiers along with their
      contents. *)

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

val channel_of_frame: 'a frame -> 'a channel
  (** Get the channel of a framed channel. *)
