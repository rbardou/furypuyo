(** Semi-persistent matrix *)

type 'a t

val make: int -> int -> 'a -> 'a t
val get: 'a t -> int -> int -> 'a
val set: 'a t -> int -> int -> 'a -> 'a t
val width: 'a t -> int
val height: 'a t -> int
val inside: 'a t -> int -> int -> bool
