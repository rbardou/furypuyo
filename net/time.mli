(** Time utilities *)

type t
  (** The type of dates. *)

type d
  (** The type of delays. *)

(** Dates and delays are quite precise.
    Resolution is usually better than 1ms. *)

val now: unit -> t
  (** Get the current date. *)

val shift: t -> d -> t
  (** Shift a date by some delay. *)

val multf: d -> float -> d
  (** Multiply a delay by a float. *)

val ge: t -> t -> bool
  (** Greater or equal comparison operator. *)

val ms: int -> d
  (** Make a delay in milliseconds. *)
