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
  ds_counts: int list;
}

type gameover_state = {
  gos_start: int;
}

type garbage_state = {
  gs_last_delay: int;
  gs_next: int;
  gs_remaining: int list list;
}

type state =
  | Normal of normal_state
  | Heaping of heaping_state
  | Delete of delete_state
  | GameOver of gameover_state
  | Garbage of garbage_state

type game = {
  now: int;
  field: Cell.t Matrix.t;
  incb: Block.t;
  incx: int;
  incy: int;
  rand: Rand.t;
  generator: Generator.t;
  state: state;
  inc_delay: int;
  inc_effect: Puyo.effect;
  score: int;
  chain: int;
}

let chain_factor game = 2 * game.chain * game.chain -4

let score_to_add game counts =
  let scores = List.map begin fun p ->
    10 * p, p + chain_factor game
  end counts in
  List.fold_left (fun (a, b) (c, d) -> a + c, b + d) (0, 0) scores

let start () =
  let generator = Generator.nice [ Red; Green; Blue; Yellow ] in
  let rand = Rand.self_init () in
  let rand, generator, incoming = Generator.next generator rand in
  let inc_delay = 100 in
  {
    now = 0;
    field = Matrix.make width height Cell.empty;
    incb = incoming;
    incx = 2;
    incy = 0;
    rand = rand;
    generator = generator;
    state = Normal { ns_next_gravity = inc_delay };
    inc_delay = inc_delay;
    inc_effect = moving_effect 0 0 1 inc_delay;
    score = 0;
    chain = 0;
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
      | Some puyo, Some p ->
          puyo.color = p.color && p.color <> Gray
      | None, _ | _, None -> false
  in
  let big_groups = ref [] in
  let counts = ref [] in
  for x = 0 to w - 1 do
    for y = 0 to h - 1 do
      let n = 2*(x+y*h) in
      let count = mark n (is_color_of x y) x y in
      unmark ();
      if count >= 4 then begin
        big_groups := (n+2) :: !big_groups;
        counts := count :: !counts;
      end
    done
  done;
  let result = ref [] in
  let is_in_big_group x y =
    Matrix.inside f x y && List.mem m.(x).(y) !big_groups in
  for x = 0 to w - 1 do
    for y = 0 to h - 1 do
      let condition =
        if Cell.is_gray (Matrix.get f x y) then
          is_in_big_group (x-1) y
          || is_in_big_group x (y+1)
          || is_in_big_group (x+1) y
          || is_in_big_group x (y-1)
        else
          is_in_big_group x y
      in
      if condition then result := (x, y) :: !result;
    done
  done;
  !result, !counts

let delete_big_groups game =
  let bg, counts = matrix_big_groups game.field in
  if bg = [] then
    game
  else
    { game with
        state = Delete { ds_cells = bg;
                         ds_delay = 60;
                         ds_counts = counts };
        chain = game.chain + 1 }

let heap_gravity_gen now delay field =
  let rec cell modified x y f =
    if y < 0 then modified, f else
      let modified, f =
        let cur = Matrix.get f x y in
        let bot = Matrix.get f x (y+1) in
        if not (Cell.is_empty cur) && Cell.is_empty bot then
          let cur =
            Cell.apply_puyo_effect (moving_effect now 0 1 delay) cur in
          let f = Matrix.set f x (y+1) cur in
          true, Matrix.set f x y Cell.empty
        else
          modified, f
      in
      cell modified x (y-1) f
  in
  let rec col modified x f =
    if x < 0 then modified, f else
      let modified, f = cell modified x (Matrix.height field - 2) f in
      col modified (x-1) f
  in
  col false (Matrix.width field - 1) field

let heap_gravity game =
  let delay = match game.state with
    | Heaping hs -> max 1 (hs.hs_last_delay - 2)
    | _ -> 12
  in
  let modified, f = heap_gravity_gen game.now delay game.field in
  if modified then
    { game with
        field = f;
        state = Heaping { hs_next_gravity = delay;
                          hs_last_delay = delay } }
  else
    let game =
      { game with state = Normal { ns_next_gravity = game.inc_delay } } in
    delete_big_groups game

let really_delete game ds =
  let cells = ds.ds_cells in
  let f =
    List.fold_left
      (fun f (x, y) -> Matrix.set f x y Cell.empty)
      game.field
      cells
  in
  let sa, sb = score_to_add game ds.ds_counts in
  let game =
    { game with
        field = f;
        score = game.score + sa * sb }
  in
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
        incy = 0;
        inc_effect = moving_effect game.now 0 1 game.inc_delay;
        chain = 0 }
  in
  heap_gravity game

let move fall x y game =
  if Block.collision game.incb (game.incx+x) (game.incy+y) game.field then
    if fall then
      insert_incoming game
    else
      game
  else
    let game =
      { game with
          inc_effect =
          if fall then moving_effect game.now x y game.inc_delay
          else game.inc_effect;
          incx = game.incx+x;
          incy = game.incy+y }
    in
    if fall then
      match game.state with
        | Normal _ ->
            { game with
                state = Normal { ns_next_gravity = game.inc_delay } }
        | _ -> game
    else game

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

let garbage lines rem game =
  let full_line =
    Array.to_list (Array.init (Matrix.width game.field) (fun i -> i)) in
  let rec full = function
    | 0 -> []
    | n -> full_line :: full (n-1)
  in
  let glines = full lines in
  let delay = 10 in
  let gs = {
    gs_last_delay = delay;
    gs_next = delay;
    gs_remaining = glines;
  } in
  { game with state = Garbage gs }

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
          | InstaFall ->
              garbage 4 3 game
        end
    | Heaping _
    | Delete _
    | GameOver _
    | Garbage _ ->
        begin match action with
          | Quit ->
              IO.quit ();
              exit 0
          | _ ->
              game
        end

let check_game_over game =
  match game.state with
    | Normal _ ->
        if not (Cell.is_empty (Matrix.get game.field 2 2))
          || not (Cell.is_empty (Matrix.get game.field 3 2)) then
            { game with state = GameOver { gos_start = game.now } }
        else
          game
    | _ -> game

let garbage_line gs game =
  let delay = max 1 (gs.gs_last_delay - 2) in
  let modified, field = heap_gravity_gen game.now delay game.field in
  match gs.gs_remaining with
    | l :: rem ->
        let gs = {
          gs_last_delay = delay;
          gs_next = delay;
          gs_remaining = rem;
        } in
        let field =
          List.fold_left
            (fun f x -> Matrix.set f x 0 (Cell.make Puyo.gray))
            field l
        in
        { game with
            state = Garbage gs;
            field = field }
    | [] ->
        if modified then
          let gs = {
            gs_last_delay = delay;
            gs_next = delay;
            gs_remaining = [];
          } in
          { game with state = Garbage gs; field = field }
        else
          { game with state = Normal { ns_next_gravity = game.inc_delay } }

let think game =
  let game = check_game_over game in
  let game = { game with now = game.now + 1 } in
  match game.state with
    | Normal ns ->
        if ns.ns_next_gravity <= 0 then
          move true 0 1
            { game with
                state = Normal { ns_next_gravity = game.inc_delay } }
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
          really_delete game ds
        else
          { game with state = Delete { ds with ds_delay = ds.ds_delay - 1 } }
    | Garbage gs ->
        if gs.gs_next <= 0 then
          garbage_line gs game
        else
          { game with state = Garbage { gs with gs_next = gs.gs_next - 1 } }
    | GameOver _ ->
        game
