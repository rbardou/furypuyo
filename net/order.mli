(** Force packet reception in order *)

open Frame

type 'a orderer
  (** The type of automatically reordering reception buffers. *)

val start: ?size: int -> 'a frame -> 'a orderer
  (** Make a reordering reception buffer from a frame.

      @param size the maximum number of buffered messages. If the last delivered
      message has identifier [id], the buffer may only contain messages with
      identifiers [id + 1], ..., [id + size]. For instance, with a size of [1],
      if [id] has not been received,
      [id + 1] is waiting, and [id + 3] is received, [id] will never be ignored
      and [id + 1] will be delivered. Now we wait for [id + 2]. With a size
      of [0], messages are instantly delivered, missing messages being ignored.
      If [size] is bigger than the size of the frame, it is adjusted to this
      size. Default size is infinite, i.e. the size of the frame, to ensure
      that all packets are received if a resending protocol is used.
      Bigger sizes not only require bigger buffers,
      they also increase latency. *)

val receive: 'a orderer -> 'a list
  (** Receive data in the same order it has been sent. *)
