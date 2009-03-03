type state = int

type t = Rand.t -> state -> Rand.t * state * Block.t

val random: Puyo.color list -> t
