open Puyo
open Action
open Cell

let width = 6
let height = 14

type normal_state = {
  ns_next_gravity: int;
}

type heaping_state = {
  hs_last_delay: int;
  hs_next_gravity: int;
}

type delete_state = {
  ds_cells: (int * int) list;
  ds_delay: int;
}

type state =
  | Normal of normal_state
  | Heaping of heaping_state
  | Delete of delete_state
  | GameOver

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
  let generator = Generator.nice [ Red; Green; Blue; Yellow ] in
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

let matrix_big_groups f =
  let w = Matrix.width f and h = Matrix.height f in
  let m = Array.init w (fun _ -> Array.make h 0) in
  let rec mark n c x y =
    let mark = mark n c in
    if Matrix.inside f x y && m.(x).(y) = 0 then begin
      m.(x).(y) <- n + 1;
      if c (Matrix.get f x y) then begin
        m.(x).(y) <- n + 2;
        1 + mark (x+1) y + mark (x-1) y + mark x (y+1) + mark x (y-1)
      end else 0
    end else 0
  in
  let unmark () =
    for x = 0 to w - 1 do
      for y = 0 to h - 1 do
        let v = m.(x).(y) in
        if v mod 2 = 1 then
          m.(x).(y) <- 0
      done
    done
  in
  let is_color_of x y p =
    match (Matrix.get f x y).puyo, p.puyo with
      | Some puyo, Some p -> puyo.color = p.color
      | None, _ | _, None -> false
  in
  let big_groups = ref [] in
  for x = 0 to w - 1 do
    for y = 0 to h - 1 do
      let n = 2*(x+y*h) in
      let count = mark n (is_color_of x y) x y in
      unmark ();
      if count >= 4 then
        big_groups := (n+2) :: !big_groups;
    done
  done;
  let result = ref [] in
  for x = 0 to w - 1 do
    for y = 0 to h - 1 do
      if List.mem m.(x).(y) !big_groups then
        result := (x, y) :: !result;
    done
  done;
  !result

let delete_big_groups game =
  let bg = matrix_big_groups game.field in
  if bg = [] then
    game
  else
    { game with state = Delete { ds_cells = bg; ds_delay = 50 } }

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
  let delay = match game.state with
    | Heaping hs -> max 1 (hs.hs_last_delay - 2)
    | _ -> 10
  in
  if modified then
    { game with
        field = f;
        state = Heaping { hs_next_gravity = delay;
                          hs_last_delay = delay } }
  else
    let game = { game with state = Normal { ns_next_gravity = 100 } } in
    delete_big_groups game

let really_delete game cells =
  let f =
    List.fold_left
      (fun f (x, y) -> Matrix.set f x y Cell.empty)
      game.field
      cells
  in
  let game = { game with field = f } in
  heap_gravity game

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
          if Block.collision newb game.incx (game.incy+1) game.field then
            game
          else
            { game with incy = game.incy+1; incb = newb }
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
    | Heaping _
    | Delete _
    | GameOver ->
        begin match action with
          | Quit ->
              IO.quit ();
              exit 0
          | _ ->
              game
        end

let sf = IO.Text.load "data/pouyou.ttf" 20

let check_game_over game =
  match game.state with
    | Normal _ ->
        if not (Cell.is_empty (Matrix.get game.field 2 2))
          || not (Cell.is_empty (Matrix.get game.field 3 2)) then
            { game with state = GameOver }
        else
          game
    | _ -> game

let think game =
  let game = check_game_over game in
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
              state =
              Heaping { hs with hs_next_gravity = hs.hs_next_gravity - 1 } }
    | Delete ds ->
        if ds.ds_delay <= 0 then
          really_delete game ds.ds_cells
        else
          { game with state = Delete { ds with ds_delay = ds.ds_delay - 1 } }
    | GameOver ->
        game
