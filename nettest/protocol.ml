type message =
  | Unit
  | String of string (* important *)
  | Int of int (* ordered *)
  | Bool of bool (* ordered, important *)

let ordered = function
  | Int _ | Bool _ -> Some 0
  | Unit | String _ -> None

let important = function
  | Unit | Int _ -> true
  | String _ | Bool _ -> false

let order_count = 1
