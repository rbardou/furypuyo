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
    incx = 2;
    incy = 0;
    rand = rand;
    generator = generator;
    next_down = 100;
  }

let move x y game =
  if Block.collision game.incb (game.incx+x) (game.incy+y) game.field then
    game
  else
    { game with incx = game.incx+x; incy = game.incy+y }

let transform f game =
  let newb = f game.incb in
  if Block.collision newb game.incx game.incy game.field then
    game
  else
    { game with incb = newb }

let act game = function
  | Quit ->
      IO.quit ();
      exit 0
  | MLeft ->
      move (-1) 0 game
  | MRight ->
      move 1 0 game
  | MDown ->
      move 0 1 game
  | RLeft ->
      transform Block.rotate_left game
  | RRight ->
      transform Block.rotate_right game

let think game =
  if game.next_down <= 0 then
    { (move 0 1 game) with next_down = 100 }
  else
    { game with next_down = game.next_down - 1 }
