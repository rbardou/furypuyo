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

let insert_incoming game =
  let rand, generator, incb = Generator.next game.generator game.rand in
  { game with
      field = Block.insert game.incb game.incx game.incy game.field;
      rand = rand;
      generator = generator;
      incb = incb;
      incx = 2;
      incy = 0 }

let move insert x y game =
  if Block.collision game.incb (game.incx+x) (game.incy+y) game.field then
    if insert then
      insert_incoming game
    else
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
      move false (-1) 0 game
  | MRight ->
      move false 1 0 game
  | MDown ->
      move true 0 1 game
  | RLeft ->
      transform Block.rotate_left game
  | RRight ->
      transform Block.rotate_right game

let think game =
  if game.next_down <= 0 then
    { (move true 0 1 game) with next_down = 100 }
  else
    { game with next_down = game.next_down - 1 }
