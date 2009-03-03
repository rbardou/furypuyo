type state = int

type t

val random: Puyo.color list -> t

val next: t -> Rand.t -> Rand.t * t * Block.t
