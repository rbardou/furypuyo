type t = {
  puyo: Puyo.t option;
  (* animations: ... *)
}

let empty = {
  puyo = None;
}
