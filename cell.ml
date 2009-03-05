open Puyo

type t = {
  puyo: Puyo.t option;
  (* animations: ... *)
}

let empty = {
  puyo = None;
}

let make p = {
  puyo = Some p;
}

let is_empty c = c.puyo = None

let is_gray c =
  match c.puyo with
    | None -> false
    | Some p -> p.color = Gray

let apply_puyo_effect e c =
  match c.puyo with
    | None -> c
    | Some p -> { puyo = Some { p with Puyo.effect = e } }

let puyo c =
  match c.puyo with
    | None -> raise (Invalid_argument "Cell.puyo")
    | Some p -> p
