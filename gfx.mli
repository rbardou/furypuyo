(** Graphic effects *)

type t =
  |  ClearScreen (** Player cleared screen. *)
  (** Graphic effect kinds. *)

type set
  (** Graphic effect set. *)

val empty: set
  (** Empty graphic effect set. *)

val add: set -> t -> int -> t
  (** Add an effect to a set.

[add set effect ending]: add [effect] to [set]. The effect will be removed
after an [update] with time greater or equal than [ending]. *)

val update: set -> int -> set
  (** Remove effects which are over.

[update set now]: remove effects whose ending time has passed. *)

val iter: (t -> unit) -> set -> unit
  (** Iteration over a set of graphic effects. *)

val map: (t -> t) -> set -> set
  (** Map a function to the effects of a set. *)
