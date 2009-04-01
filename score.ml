(** Game scores *)

type t = int

let compare = compare

let codec = Bin.int
