(** Resend unacknowledged packets *)

(** Allows to resend unacknowledged packets that are still in the
    sending frame. *)

open Frame

type 'a sender
  (** The type of automatically resending buffers. *)

val start: 'a frame -> 'a sender
  (** Make a resending buffer from a frame. *)

val send: 'a sender -> 'a -> unit
  (** Send data that will be resent if needed. *)

val update: 'a sender -> unit
  (** Resend data that should be resent.

      Data that should be resent are unacknowledged messages still in the
      sending frame and that have not been resent for some time. This delay
      is calculated automatically and grows as the message is resent. *)
