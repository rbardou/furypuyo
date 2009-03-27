type t = float

type d = float

let now () = Unix.gettimeofday ()

let shift t d = t +. d

let ge = (>=)

let ms x = float_of_int x /. 1000.

let multf t d = t *. d
