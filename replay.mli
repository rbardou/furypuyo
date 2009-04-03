(** Game replays *)

type t
  (** The type of replays. *)

val record: Game.game -> t
  (** Start recording a game. *)

val frame: t -> Action.t list -> unit
  (** Record a new frame. *)

val play: t -> Game.game
  (** Start playing a game replay. *)

val next: t -> Game.game
  (** Get the next frame of a replay. *)

val codec: t Bin.t
  (** Codec of game replays. *)
