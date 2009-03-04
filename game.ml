open Puyo
open Action

let width = 6
let height = 14

type normal_state = {
  ns_next_gravity: int;
}

type heaping_state = {
  hs_next_gravity: int;
}

type state =
  | Normal of normal_state
  | Heaping of heaping_state

type game = {
  field: Cell.t Matrix.t;
  incb: Block.t;
  incx: int;
  incy: int;
  rand: Rand.t;
  generator: Generator.t;
  state: state;
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
    state = Normal { ns_next_gravity = 100 };
  }

let heap_gravity game =
  let rec cell modified x y f =
    if y < 0 then modified, f else
      let modified, f =
        let cur = Matrix.get f x y in
        let bot = Matrix.get f x (y+1) in
        if not (Cell.is_empty cur) && Cell.is_empty bot then
          let f = Matrix.set f x (y+1) cur in
          true, Matrix.set f x y Cell.empty
        else
          modified, f
      in
      cell modified x (y-1) f
  in
  let rec col modified x f =
    if x < 0 then modified, f else
      let modified, f = cell modified x (Matrix.height game.field - 2) f in
      col modified (x-1) f
  in
  let modified, f = col false (Matrix.width game.field - 1) game.field in
  if modified then
    { game with
        field = f;
        state = Heaping { hs_next_gravity = 10 } }
  else
    { game with
        state = Normal { ns_next_gravity = 100 } }

let insert_incoming game =
  let rand, generator, incb = Generator.next game.generator game.rand in
  let game =
    { game with
        field = Block.insert game.incb game.incx game.incy game.field;
        rand = rand;
        generator = generator;
        incb = incb;
        incx = 2;
        incy = 0 }
  in
  heap_gravity game

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
    if Block.collision newb (game.incx-1) game.incy game.field then
      if Block.collision newb (game.incx+1) game.incy game.field then
        if Block.collision newb game.incx (game.incy-1) game.field then
          game
        else
          { game with incy = game.incy-1; incb = newb }
      else
        { game with incx = game.incx+1; incb = newb }
    else
      { game with incx = game.incx-1; incb = newb }
  else
    { game with incb = newb }

let act game action =
  match game.state with
    | Normal _ ->
        begin match action with
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
        end
    | Heaping _ ->
        game

let think game =
  match game.state with
    | Normal ns ->
        if ns.ns_next_gravity <= 0 then
          move true 0 1
            { game with
                state = Normal { ns_next_gravity = 100 } }
        else
          { game with
              state = Normal { ns_next_gravity = ns.ns_next_gravity - 1 } }
    | Heaping hs ->
        if hs.hs_next_gravity <= 0 then
          heap_gravity game
        else
          { game with
              state = Heaping { hs_next_gravity = hs.hs_next_gravity - 1 } }
