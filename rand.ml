type t = int

let self_init () =
  Random.self_init ();
  0

let int r bound =
  r, Random.int bound
