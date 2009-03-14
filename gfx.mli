(** Graphic effects *)

type particle_sprite =
  | GreenStar
  | YellowStar
  | RedStar
  | PurpleStar

type particle = {
  sprite: particle_sprite;
  cx: int;
  cy: int;
  mutable x: float;
  mutable y: float;
  mutable vx: float;
  mutable vy: float;
  ax: float;
  ay: float;
}

type t =
  | ClearScreen (** Player cleared screen. *)
  | Particle of particle (** Particle effect. *)
  (** Graphic effect kinds. *)

type set
  (** Graphic effect set. *)

val empty: set
  (** Empty graphic effect set. *)

val add: set -> t -> int -> set
  (** Add an effect to a set.

[add set effect ending]: add [effect] to [set]. The effect will be removed
after an [update] with time greater or equal than [ending]. *)

val remove: set -> int -> set
  (** Remove effects of a given ending time.

[remove set now]: remove effects whose ending time is [now]. Effects whose
ending time is strictly less than [now] are not removed. *)

val iter: (t -> unit) -> set -> unit
  (** Iteration over a set of graphic effects. *)

val map: (t -> t) -> set -> set
  (** Map a function to the effects of a set. *)
