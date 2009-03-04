(** Blocks *)

type t =
  | List1 of (int * int * Puyo.t) list
      (** Center of rotation at [(0, 0)]. *)
  | List2 of (int * int * Puyo.t) list
      (** Center of rotation at [(0.5, 0.5)]. *)
  | Quad of Puyo.color * Puyo.color list
      (** Current color and next colors. *)

val rotate_left: t -> t

val rotate_right: t -> t

val collision: t -> int -> int -> Cell.t Matrix.t -> bool

val insert: t -> int -> int -> Cell.t Matrix.t -> Cell.t Matrix.t
