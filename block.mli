(** Blocks *)

type t =
  | List of (int * int * Puyo.t) list
  | Quad of Puyo.color * Puyo.color list
      (** Current color and next colors. *)

val rotate_left: t -> t

val rotate_right: t -> t
