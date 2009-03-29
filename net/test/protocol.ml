type message =
  | Unit
  | String of string
  | Int of int
  | Bool of bool

let channels = [
  0, Net.Fast;
  1, Net.FastOrdered;
  2, Net.Important;
  3, Net.Ordered;
]

let channel = function
  | Unit -> 0
  | Bool _ -> 1
  | String _ -> 2
  | Int _ -> 3
