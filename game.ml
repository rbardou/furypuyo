open Action
open Puyo
open Cell

let smooth_factor = 1024

let unsmooth_y y =
  y / smooth_factor + (if y mod smooth_factor = 0 then 0 else 1)

let smooth_y y =
  y * smooth_factor

type incoming_state = {
  inc_block: Block.t;
  inc_x: int; (** cells *)
  inc_y: int; (** cells, multiplied by [smooth_factor] *)
  inc_insert_time: int;
}

type inserting_state = {
  ins_end: int;
}

type falling_state = {
  f_puyos: (int * int * Puyo.t) list; (** (x, original y, puyo) list *)
  f_y: int; (** smoothed y offset *)
  f_speed: int; (** smoothed y per frame *)
}

type popping_state = {
  pop_end: int;
  pop_puyos: (int * int) list;
}

type gameover_state = {
  go_bla: int;
}

type state =
  | Starting
  | Incoming of incoming_state
  | Inserting of inserting_state
  | Falling of falling_state
  | Popping of popping_state
  | GameOver of gameover_state

type speed = {
  sp_fall_absorb: int;
    (** number of smoothed fall absorbed *)
  sp_fall: int;
    (** number of smoothed cells by frame *)
  sp_fall_fast: int;
    (** number of (additional) smoothed cells by frame *)
  sp_insert_delay: int;
    (** delay the block stayed without moving when inserted *)
  sp_gravity: int;
    (** acceleration (smoothed y per frame per frame) for falling blocks *)
  sp_pop_delay: int;
    (** time puyos take to pop *)
}

type game = {
  now: int;
  field: Cell.t Matrix.t;
  rand: Rand.t;
  generator: Generator.t;
  state: state;
  score: int;
  speed: speed;
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

let start_incoming game =
  let rand, generator, block = Generator.next game.generator game.rand in
  let is = {
    inc_block = block;
    inc_x = 2;
    inc_y = 0;
    inc_insert_time = game.speed.sp_fall_absorb;
  } in
  { game with
      state = Incoming is;
      rand = rand;
      generator = generator }

let start_inserting game block x y =
  let new_field = Block.insert block x y game.field in
  let is = {
    ins_end = game.now + game.speed.sp_insert_delay;
  } in
  { game with
      state = Inserting is;
      field = new_field }

let start_falling game puyos =
  let fs = {
    f_puyos = puyos;
    f_y = 0;
    f_speed = 0;
  } in
  { game with state = Falling fs }

(** return puyos from top to bottom *)
let extract_falling_puyos field =
  let rec cell puyos x y f =
    if y < 0 then puyos, f else
      let puyos, f =
        let cur = Matrix.get f x y in
        let bot = Matrix.get f x (y+1) in
        if not (Cell.is_empty cur) && Cell.is_empty bot then
          (x, y, Cell.puyo cur) :: puyos, Matrix.set f x y Cell.empty
        else
          puyos, f
      in
      cell puyos x (y-1) f
  in
  let rec col puyos x f =
    if x < 0 then puyos, f else
      let puyos, f = cell puyos x (Matrix.height field - 2) f in
      col puyos (x-1) f
  in
  col [] (Matrix.width field - 1) field

let start_popping game puyos =
  let ps = {
    pop_end = game.now + game.speed.sp_pop_delay;
    pop_puyos = puyos;
  } in
  { game with state = Popping ps }

let check_and_start_popping game =
  let puyos, _ = matrix_big_groups game.field in
  match puyos with
    | [] ->
        start_incoming game
    | _ ->
        start_popping game puyos

let check_and_start_chain game =
  match extract_falling_puyos game.field with
    | [], _ ->
        check_and_start_popping game
    | puyos, field ->
        start_falling { game with field = field } (List.rev puyos)

let fall game is speed =
  let new_y = is.inc_y + speed in
  let real_new_y = unsmooth_y new_y in
  let final_y, new_insert_time =
    if Block.collision is.inc_block is.inc_x real_new_y game.field then
      let final_y = smooth_y (real_new_y - 1) in
      final_y, is.inc_insert_time - (new_y - final_y)
    else
      new_y, is.inc_insert_time
  in
  if new_insert_time <= 0 then
    start_inserting game is.inc_block is.inc_x (unsmooth_y final_y)
  else
    let is = {
      is with
        inc_y = final_y;
        inc_insert_time = new_insert_time;
    } in
    { game with state = Incoming is }

let think_incoming game is =
  fall game is game.speed.sp_fall

let think_inserting game is =
  if game.now >= is.ins_end then check_and_start_chain game
  else game

let think_falling game fs =
  let rec fall_puyos field puyos = function
    | [] ->
        field, puyos
    | ((x, y, puyo) as p) :: rem ->
        let new_y = smooth_y y + fs.f_y in
        let real_new_y = unsmooth_y new_y in
        if not (Matrix.inside game.field x real_new_y &&
                  Cell.is_empty (Matrix.get field x real_new_y)) then
          let cell = Cell.make puyo in
          let new_field =
            Matrix.set field x (real_new_y - 1) cell in
          fall_puyos new_field puyos rem
        else begin
          fall_puyos field (p :: puyos) rem
        end
  in
  let field, puyos = fall_puyos game.field [] fs.f_puyos in
  let game = { game with field = field } in
  match puyos with
    | [] ->
        check_and_start_popping game
    | _ ->
        let new_speed =
          min (smooth_factor - 1) (fs.f_speed + game.speed.sp_gravity) in
        let fs = {
          f_puyos = puyos;
          f_speed = new_speed;
          f_y = fs.f_y + fs.f_speed;
        } in
        { game with state = Falling fs }

let pop_puyos field puyos =
  List.fold_left
    (fun f (x, y) -> Matrix.set f x y Cell.empty)
    field puyos

let think_popping game ps =
  if game.now >= ps.pop_end then
    let field = pop_puyos game.field ps.pop_puyos in
    check_and_start_chain { game with field = field }
  else game

let quit () =
  IO.quit ();
  exit 0

let move game is delta =
  let new_x = is.inc_x + delta in
  let real_y = unsmooth_y is.inc_y in
  if Block.collision is.inc_block new_x real_y game.field then
    game
  else
    let is = { is with inc_x = new_x } in
    { game with state = Incoming is }

let rotate rotate_fun game is =
  try
    let b = is.inc_block and x = is.inc_x and y = unsmooth_y is.inc_y in
    let b1 = rotate_fun b in
    let b, x =
      List.find
        (fun (b, x) -> not (Block.collision b x y game.field))
        [
          b1, x;
          b1, x - 1;
          b1, x + 1;
          rotate_fun b1, x;
        ]
    in
    let is = { is with inc_block = b; inc_x = x } in
    { game with state = Incoming is }
  with Not_found ->
    game

let insta_fall game is =
  let b = is.inc_block in
  let x = is.inc_x in
  let rec find_y y =
    if Block.collision b x y game.field then y-1
    else find_y (y+1)
  in
  let y = find_y (unsmooth_y is.inc_y) in
  start_inserting game b x y

let debug game =
  let f = game.field in
  let f = Matrix.set f 0 13 (Cell.make (Puyo.make Yellow)) in
  let f = Matrix.set f 1 13 (Cell.make (Puyo.make Yellow)) in
  let f = Matrix.set f 2 13 (Cell.make (Puyo.make Yellow)) in
  let f = Matrix.set f 0 12 (Cell.make (Puyo.make Green)) in
  let f = Matrix.set f 1 12 (Cell.make (Puyo.make Green)) in
  let f = Matrix.set f 2 12 (Cell.make (Puyo.make Yellow)) in
  let f = Matrix.set f 0 11 (Cell.make (Puyo.make Yellow)) in
  let f = Matrix.set f 1 11 (Cell.make (Puyo.make Yellow)) in
  let f = Matrix.set f 2 11 (Cell.make (Puyo.make Yellow)) in
  { game with field = f }

let act_incoming game is = function
  | Quit -> quit ()
  | MLeft -> move game is (-1)
  | MRight -> move game is 1
  | MDown -> fall game is game.speed.sp_fall_fast
  | RLeft -> rotate Block.rotate_left game is
  | RRight -> rotate Block.rotate_right game is
  | InstaFall -> insta_fall game is
  | Debug -> debug game

let act_quit game = function
  | Quit -> quit ()
  | _ -> game

let act game input =
  match game.state with
    | Starting
    | Inserting _
    | Falling _
    | Popping _
    | GameOver _ -> act_quit game input
    | Incoming is -> act_incoming game is input

let think game =
  let game = { game with now = game.now + 1 } in
  match game.state with
    | Starting -> start_incoming game
    | Incoming is -> think_incoming game is
    | Inserting is -> think_inserting game is
    | Falling fs -> think_falling game fs
    | Popping ps -> think_popping game ps
    | _ -> assert false (* TODO *)

let start () =
  let generator = Generator.nice [ Red; Green; Blue; Yellow ] in
  let rand = Rand.self_init () in
  {
    now = 0;
    field = Matrix.make 6 14 Cell.empty;
    rand = rand;
    generator = generator;
    state = Starting;
    score = 0;
    speed = {
      sp_fall_absorb = 100;
      sp_fall = 20;
      sp_fall_fast = 300;
      sp_insert_delay = 10;
      sp_gravity = 10;
      sp_pop_delay = 40;
    };
  }

(* bug

YYY
BBY
YYY
---

becomes

 B
---

One blue has disappeared

(actually B was green) *)
