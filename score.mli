(** Game scores *)

type t

val codec: t Bin.t

val compare: t -> t -> int

val make: int -> t

val score: t -> int
