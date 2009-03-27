(** Channels over client /server connections *)

open Connect

type 'a m
  (** The type of messages transmitted by a channel on a connection. *)

type 'a channel
  (** The type of connections. *)

val channel: 'a m connection -> int -> 'a channel
  (** Get a channel of a connection. *)

val send: 'a channel -> 'a -> unit
  (** Send data over a channel. *)

val receive: 'a channel -> 'a list
  (** Receive data over a channel. *)
