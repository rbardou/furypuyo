type color = Red | Green | Blue | Yellow

type t = {
  color: color;
  (* mimic: ... *)
}

let make color = {
  color = color;
}

let red = {
  color = Red;
}

let yellow = {
  color = Yellow;
}

let blue = {
  color = Blue;
}

let green = {
  color = Green;
}
