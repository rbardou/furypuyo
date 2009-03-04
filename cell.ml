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
