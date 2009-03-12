(** Graphic effects *)

type t =
  |  ClearScreen (** Player cleared screen. *)
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
