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
