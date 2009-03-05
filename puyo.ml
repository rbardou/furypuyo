type color = Red | Green | Blue | Yellow | Gray

type moving_effect = {
  me_start: int;
  me_sx: float; (** cell per tick *)
  me_sy: float;
  me_px: float; (** cell *)
  me_py: float;
  me_end: int;
}

type effect =
  | NoEffect
  | Moving of moving_effect

type t = {
  color: color;
  effect: effect;
  (* mimic: ... *)
}

let moving_effect now x y delay =
  let x = float_of_int x in
  let y = float_of_int y in
  let fdelay = if delay = 0 then 1. else float_of_int delay in
  Moving {
    me_start = now;
    me_sx = x /. fdelay;
    me_sy = y /. fdelay;
    me_px = -.x;
    me_py = -.y;
    me_end = now + delay;
  }

let make color = {
  color = color;
  effect = NoEffect;
}

let gray = make Gray

let to_string p =
  match p.color with
    | Red -> "Red"
    | Green -> "Green"
    | Blue -> "Blue"
    | Yellow -> "Yellow"
    | Gray -> "Gray"
