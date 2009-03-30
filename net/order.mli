(** Force packet reception in order *)

open Frame

type 'a orderer
  (** The type of automatically reordering reception buffers. *)

val start: ?size: int -> 'a frame -> 'a orderer
  (** Make a reordering reception buffer from a frame.

      The ordered ensures that if [id] is delivered, a message before [id] can
      no longer be delivered. Thus, messages are delivered in the order they
      are sent.

      Before [id] is delivered, we wait so that [id - 1], ..., [id - size] are
      delivered.

      If [size] is [0], messages are instantly delivered, and previous
      messages are lost. A [size] greater or equal to the size of the
      frame minus one (default value) suffice to ensure that all packets are
      received if the sending frame is never shifted by remote peer unless
      messages are acknowledged (default behavior). *)

val receive: 'a orderer -> 'a list
  (** Receive data in the same order it has been sent. *)
