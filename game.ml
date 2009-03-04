open Puyo
open Action

let width = 6
let height = 14

type game = {
  field: Cell.t Matrix.t;
  incb: Block.t;
  incx: int;
  incy: int;
  rand: Rand.t;
  generator: Generator.t;
  next_down: int;
}

let start () =
  let generator = Generator.random [ Red; Green; Blue; Yellow ] in
  let rand = Rand.self_init () in
  let rand, generator, incoming = Generator.next generator rand in
  {
    field = Matrix.make width height Cell.empty;
    incb = incoming;
    incx = 3;
    incy = 1;
    rand = rand;
    generator = generator;
    next_down = 100;
  }

let act game = function
  | Quit ->
      IO.quit ();
      exit 0
  | Left ->
      { game with incx = game.incx - 1 }
  | Right ->
      { game with incx = game.incx + 1 }
  | Up ->
      { game with incb = Block.rotate_left game.incb }
  | Down ->
      { game with incb = Block.rotate_right game.incb }

let think game =
  if game.next_down <= 0 then
    { game with
        incy = game.incy + 1;
        next_down = 100 }
  else
    { game with next_down = game.next_down - 1 }
