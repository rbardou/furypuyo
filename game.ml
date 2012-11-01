(**************************************************************************)
(* Copyright (c) 2009, Romain BARDOU                                      *)
(* All rights reserved.                                                   *)
(*                                                                        *)
(* Redistribution and  use in  source and binary  forms, with  or without *)
(* modification, are permitted provided that the following conditions are *)
(* met:                                                                   *)
(*                                                                        *)
(* * Redistributions  of  source code  must  retain  the above  copyright *)
(*   notice, this list of conditions and the following disclaimer.        *)
(* * Redistributions in  binary form  must reproduce the  above copyright *)
(*   notice, this list of conditions  and the following disclaimer in the *)
(*   documentation and/or other materials provided with the distribution. *)
(* * Neither the name of Fury Puyo nor  the names of its contributors may *)
(*   be used  to endorse or  promote products derived  from this software *)
(*   without specific prior written permission.                           *)
(*                                                                        *)
(* THIS SOFTWARE  IS PROVIDED BY  THE COPYRIGHT HOLDERS  AND CONTRIBUTORS *)
(* "AS  IS" AND  ANY EXPRESS  OR IMPLIED  WARRANTIES, INCLUDING,  BUT NOT *)
(* LIMITED TO, THE IMPLIED  WARRANTIES OF MERCHANTABILITY AND FITNESS FOR *)
(* A PARTICULAR PURPOSE  ARE DISCLAIMED. IN NO EVENT  SHALL THE COPYRIGHT *)
(* OWNER OR CONTRIBUTORS BE  LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, *)
(* SPECIAL,  EXEMPLARY,  OR  CONSEQUENTIAL  DAMAGES (INCLUDING,  BUT  NOT *)
(* LIMITED TO, PROCUREMENT OF SUBSTITUTE  GOODS OR SERVICES; LOSS OF USE, *)
(* DATA, OR PROFITS; OR BUSINESS  INTERRUPTION) HOWEVER CAUSED AND ON ANY *)
(* THEORY OF  LIABILITY, WHETHER IN  CONTRACT, STRICT LIABILITY,  OR TORT *)
(* (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING  IN ANY WAY OUT OF THE USE *)
(* OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.   *)
(**************************************************************************)

(** Game state and rules *)

open Misc
open Action
open Puyo
open Cell

let ready_set_go_delay = 75

(** Lines above the field where the user can move, rotate and place puyos,
    but without seeing them. *)
let invisible_lines = 3

(** Puyos which are placed in the first [no_puyo_lines] lines
    (which are included in the invisible lines) are destroyed when they
    are placed.

    This should be lesser or equal that [invisible_lines], as these puyo
    should not be taken into account when looking for puyo connections. *)
let no_puyo_lines = 2

let smooth_factor = 1024

let incoming_blocks_origin_x = 2
let incoming_blocks_origin_y = smooth_factor

let initial_move_key_repeat_delay = 12
let move_key_repeat_delay = 3
let score_per_garbage = 70 (* Puyo Tsu Rule. Old FuryPuyo value is 120. *)

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
  ins_block: Block.t;
  ins_x: int; (** cells *)
  ins_y: int; (** cells *)
}

type falling_state = {
  f_puyos: (int * int * Puyo.t) list; (** (x, original y, puyo) list *)
  f_y: int; (** smoothed y offset *)
  f_speed: int; (** smoothed y per frame *)
}

type popping_state = {
  pop_end: int;
  pop_puyos: (int * int) list;
  pop_score_base: int;
  pop_score_mult: int;
  pop_chain: int;
}

type gameover_state = {
  go_speed: int;
  go_y: int; (** smoothed *)
  go_end: int;
}

type starting_state = {
  s_countdown: int;
  s_next: int;
}

type state =
  | Starting of starting_state
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
  sp_fury_initial_delay: int;
    (** initial time to delete an offset *)
  sp_fury_acceleration: int;
    (** when an offset is deleted, add this to the time to delete an offset *)
  sp_fury_minimum_delay: int;
    (** minimum time to delete an offset *)
  sp_fury_initial: int;
    (** initial fury duration at maximum offsets *)
  sp_fury_gravity: int;
    (** acceleration (smoothed y per frame per frame) for falling blocks *)
  sp_fury_pop_delay: int;
    (** time puyos take to pop *)
  sp_garbage_initial: int;
    (** time before garbage starts to be sent in greater numbers *)
  sp_garbage_acceleration_delay: int;
    (** delay between two accelerations of garbage sending speed
        (one percent of normal garbage count each time)
        (0 or less means no acceleration) *)
  sp_allow_insta_fall: bool;
    (** Is hard drop allowed? *)
}

type fury_state =
  | FNone
  | FInitial of int
      (** time when this state ends *)
  | FDown of int * int
      (** time of next offset deletion, next delay to next offset deletion *)

type game = {
  now: int;
  field: Cell.t Matrix.t;
  rand: Rand.t;
  generator: Generator.t;
  state: state;
  score: int;
  speed: speed;
  chain: int;
  next_blocks: Block.t list;
  garbage_incoming: (int * int) list;
    (** garbage that can be offset but that won't fall yet
        (associates an opponent's number and garbage coming from him) *)
  garbage_ready: int;
    (** garbage ready to fall *)
  garbage_sent: int;
    (** used by online games to count how much garbage should be sent
        to other players once [garbage_finished] is [true] *)
  garbage_finished: bool;
    (** see above *)
  garbage_protection: bool;
  garbage_position: int;
  offsets: int;
  fury: fury_state;
  gfx: Gfx.set;

  (** Last presses:
      If -1, key is currently released.
      If 0, key was just downed and not taken into account.
      If more, last time key was downed - not changed until released.
      If negative, the absolute minus one is the actual value,
      and last time was initial. *)
  left_last_press: int;
  right_last_press: int;
  fast_fall: bool;
}

let garbage_protection game =
  match game.fury with
    | FNone ->
        game.garbage_protection
    | FInitial _
    | FDown _ ->
        true

let gravity game =
  match game.fury with
    | FNone ->
        game.speed.sp_gravity
    | FInitial _
    | FDown _ ->
        game.speed.sp_fury_gravity

let pop_delay game =
  match game.fury with
    | FNone ->
        game.speed.sp_pop_delay
    | FInitial _
    | FDown _ ->
        game.speed.sp_fury_pop_delay

let rec gfx_explosion game gfx sprite power x y = function
  | 0 -> gfx
  | n ->
      let angle = Random.float (2. *. 3.141592) in
      let particle = {
        Gfx.sprite = sprite;
        Gfx.cx = x;
        Gfx.cy = y;
        Gfx.x = 0.;
        Gfx.y = 0.;
        Gfx.vx = ((0.5 +. Random.float 1.) *. cos angle) *. power;
        Gfx.vy = ((0.5 +. Random.float 1.) *. sin angle) *. power;
        Gfx.ax = -0.01 *. Random.float 1. *. cos angle *. power;
        Gfx.ay = -0.01 *. Random.float 1. *. sin angle *. power;
      } in
      let life = 40 + Random.int 30 in
      let gfx = Gfx.add gfx (Gfx.Particle particle) (game.now + life) in
      gfx_explosion game gfx sprite power x y (n - 1)

let gfx_clear_screen game =
  Gfx.add game.gfx Gfx.ClearScreen (game.now + 80)

let matrix_big_groups f =
  let w = Matrix.width f and h = Matrix.height f in
  let m = Array.init w (fun _ -> Array.make h 0) in
  let rec mark n c x y =
    let mark = mark n c in
    if Matrix.inside f x y && y >= invisible_lines && m.(x).(y) = 0 then begin
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

let start_game_over game =
  let go = {
    go_speed = 0;
    go_y = 0;
    go_end = game.now + 100;
  } in
  { game with state = GameOver go }

let check_game_over game =
  let f = game.field in
  not (Cell.is_empty (Matrix.get f 2 invisible_lines))
(*       && Cell.is_empty (Matrix.get f 3 invisible_lines))*)

let start_falling game puyos =
  let fs = {
    f_puyos = puyos;
    f_y = 0;
    f_speed = 0;
  } in
  { game with state = Falling fs }

(** order: 0, 3, 4, 2, 1, 5 *)
let next_garbage_position = function
  | 0 -> 3
  | 1 -> 5
  | 2 -> 1
  | 3 -> 4
  | 4 -> 2
  | _ -> 0

(** return puyos from top to bottom *)
let make_garbage game count =
  let g = Puyo.gray in
  let w = Matrix.width game.field in
  let rec make_line acc y = function
    | 0 -> acc
    | n -> let n = n - 1 in make_line ((n, y, g) :: acc) y n
  in
  let make_line y = make_line [] y w in
  let rec make_last_line gp acc y = function
    | 0 -> gp, acc
    | n ->
        let ngp = next_garbage_position gp in
        make_last_line ngp ((ngp, y, g) :: acc) y (n - 1)
  in
  let rec make_lines acc y count =
    if count > w then
      make_lines (make_line y :: acc) (y - 1) (count - w)
    else
      let gp, line = make_last_line game.garbage_position [] y count in
      gp, line :: acc
  in
  let gp, lines = make_lines [] 1 count in
  let game = { game with garbage_position = gp } in
  game, List.flatten lines

let start_garbage game =
  let count = min 30 game.garbage_ready in
  let game = {
    game with
      garbage_ready = game.garbage_ready - count;
      garbage_protection = true;
  } in
  let game, puyos = make_garbage game count in
  start_falling game (List.rev puyos)

let start_incoming game =
  if check_game_over game then
    start_game_over game
  else if game.garbage_ready > 0 && not (garbage_protection game) then
    start_garbage game
  else
    let rand, generator, block = Generator.next game.generator game.rand in
    let next_blocks = game.next_blocks @ [ block ] in
    let block, next_blocks = match next_blocks with
      | [] -> assert false
      | x::r -> x, r
    in
    let is = {
      inc_block = block;
      inc_x = incoming_blocks_origin_x;
      inc_y = incoming_blocks_origin_y;
      inc_insert_time = game.speed.sp_fall_absorb;
    } in
    { game with
        chain = 1;
        state = Incoming is;
        rand = rand;
        generator = generator;
        next_blocks = next_blocks;
        garbage_protection = false;
        garbage_finished = true }

let start_inserting game block x y =
  let is = {
    ins_end = game.now + game.speed.sp_insert_delay;
    ins_block = block;
    ins_x = x;
    ins_y = y;
  } in
  { game with state = Inserting is }

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

(*
(* Old FuryPuyo rules *)
let chain_mult game chain =
  match chain with
    | 1 -> -2
    | 2 -> 4
    | 3 -> 12
    | 4 -> 18
    | 5 -> 29
    | 6 -> 63
    | 7 -> 107
    | 8 -> 163
    | 9 -> 119
    | 10 -> 275
    | 11 -> 345
    | 12 -> 415
    | 13 -> 485
    | 14 -> 555
    | 15 -> 625
    | _ -> 695

let start_popping game puyos groups =
  let score_base = List.fold_left (fun acc x -> acc + 10 * x) 0 groups in
  let score_mult =
    List.fold_left
      (fun acc x -> acc + chain_mult game game.chain + x)
      0 groups
  in
  let ps = {
    pop_end = game.now + pop_delay game;
    pop_puyos = puyos;
    pop_score_base = score_base;
    pop_score_mult = score_mult;
    pop_chain = game.chain;
  } in
  { game with
      state = Popping ps;
      chain = game.chain + 1;
      garbage_protection = true } (* TODO: what is that last line? *)
*)

(* Puyo Puyo Tsu rules. *)
let chain_power chain =
  match chain with
    | 1 -> 0
    | 2 -> 8
    | 3 -> 16
    | 4 -> 32
    | 5 -> 64
    | 6 -> 96
    | 7 -> 128
    | 8 -> 160
    | 9 -> 192
    | 10 -> 224
    | 11 -> 256
    | 12 -> 288
    | 13 -> 320
    | 14 -> 352
    | 15 -> 384
    | 16 -> 416
    | 17 -> 448
    | 18 -> 480
    | _ -> 512

(* Puyo Puyo Tsu rules. *)
let color_bonus color_count =
  match color_count with
    | 1 -> 0
    | 2 -> 3
    | 3 -> 6
    | _ -> 12

(* Puyo Puyo Tsu rules. *)
let group_bonus puyo_count =
  match puyo_count with
    | 4 -> 0
    | 5 -> 2
    | 6 -> 3
    | 7 -> 4
    | 8 -> 5
    | 9 -> 6
    | 10 -> 7
    | _ -> 10

let start_popping game puyos groups =
  let puyo_cleared =
    List.fold_left (+) 0 groups
  in
  let chain_power = chain_power game.chain in
  let color_bonus =
    color_bonus (List.length groups) (* TODO: count the actual colors *)
  in
  let group_bonus =
    List.fold_left (+) 0 (List.map group_bonus groups)
  in
  let score_base = 10 * puyo_cleared in
  let score_mult =
    max 1 (min 999 (chain_power + color_bonus + group_bonus))
  in
  let ps = {
    pop_end = game.now + pop_delay game;
    pop_puyos = puyos;
    pop_score_base = score_base;
    pop_score_mult = score_mult;
    pop_chain = game.chain;
  } in
  { game with
      state = Popping ps;
      chain = game.chain + 1;
      garbage_protection = true } (* TODO: what is that last line? *)

let erase_no_puyo_lines f =
  List.fold_left
    (fun f x ->
       List.fold_left
         (fun f y -> Matrix.set f x y Cell.empty)
         f
         (enumerate 0 (no_puyo_lines - 1)))
    f
    (enumerate 0 (Matrix.width f - 1))

let check_and_start_popping game =
  let game = { game with field = erase_no_puyo_lines game.field } in
  let puyos, groups = matrix_big_groups game.field in
  match puyos with
    | [] ->
        start_incoming game
    | _ ->
        start_popping game puyos groups

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
      if game.fast_fall then
        final_y, 0
      else
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

let may_press now last =
  if last = -1 then
    None (* key is not pressed *)
  else if last = 0 then
    Some (-now-1) (* first press *)
  else if last < 0 then
    if now + last + 1 > initial_move_key_repeat_delay then
      Some now
    else
      None
  else
    if now - last > move_key_repeat_delay then
      Some now
    else
      None

let move game is delta =
  let new_x = is.inc_x + delta in
  let real_y = unsmooth_y is.inc_y in
  if Block.collision is.inc_block new_x real_y game.field then
    None
  else
    Some { is with inc_x = new_x }

let think_incoming game is =
  let speed =
    if game.fast_fall then
      game.speed.sp_fall_fast
    else
      game.speed.sp_fall
  in
  let game, is =
    match may_press game.now game.left_last_press with
      | None -> game, is
      | Some time ->
          match move game is (-1) with
            | None -> game, is
            | Some is -> { game with left_last_press = time }, is
  in
  let game, is =
    match may_press game.now game.right_last_press with
      | None -> game, is
      | Some time ->
          match move game is 1 with
            | None -> game, is
            | Some is -> { game with right_last_press = time }, is
  in
  fall game is speed

let think_inserting game is =
  if game.now >= is.ins_end then
    begin
      let new_field = Block.insert is.ins_block is.ins_x is.ins_y game.field in
      let game = { game with field = new_field } in
      check_and_start_chain game
    end
  else
    game

let think_falling game fs =
  let rec fall_puyos field puyos = function
    | [] ->
        field, puyos
    | ((x, y, puyo) as p) :: rem ->
        let new_y = smooth_y y + fs.f_y in
        let real_new_y = unsmooth_y new_y in
        if not (Matrix.inside game.field x real_new_y &&
                  Cell.is_empty (Matrix.get field x real_new_y))
          && real_new_y >= 2 then
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
          min (smooth_factor - 1) (fs.f_speed + gravity game) in
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

let ceil_div x y =
  if x mod y > 0 then x / y + 1 else x / y

let check_and_start_fury offsets game =
  match game.fury with
    | FNone ->
        if offsets >= 7 then
          FInitial (game.now + game.speed.sp_fury_initial)
        else
          FNone
    | f -> f

let is_empty_field f =
  try
    for x = 0 to Matrix.width f - 1 do
      for y = 0 to Matrix.height f - 1 do
        if not (Cell.is_empty (Matrix.get f x y)) then
          raise Exit
      done
    done;
    true
  with Exit ->
    false

(*
  Score by chain count (4 puyos):
  3: 560 (0-10, 0, 0, 0)
  6: 2600 (10-15, 0-5, 0, 0)
  9: 4840 (15, 5-10, 0-5, 0)
  12: 16680 (15, 10, 5-10, 0-5)
*)
let gfx_pop game gfx puyos score =
  let g, y, r, p, pow =
    if score < 500 then
      score / 50, 0, 0, 0,
      0.5 +. float_of_int score /. 1000.
    else if score < 2500 then
      10 + (score - 500) / 400, score / 500, 0, 0,
      1. +. float_of_int (score - 500) /. 4000.
    else if score < 5000 then
      15, score / 500, (score - 2500) / 500, 0,
      1.5 +. float_of_int (score - 2500) /. 5000.
    else if score < 15000 then
      15, 10, 5 + (score - 5000) / 2000, (score - 5000) / 2000,
      2. +. float_of_int (score - 5000) /. 20000.
    else
      15, 10, 10, 5, 2.5
  in
  let puyo_count = List.length puyos in
  let random_array count =
    let a = Array.make puyo_count 0 in
    for i = 0 to count - 1 do
      let r = Random.int puyo_count in
      a.(r) <- a.(r) + 1
    done;
    a
  in
  let ga = random_array g in
  let ya = random_array y in
  let ra = random_array r in
  let pa = random_array p in
  let gfx, i =
    List.fold_left
      (fun (gfx, i) (x, y) ->
         let gfx = gfx_explosion game gfx Gfx.GreenStar pow x y ga.(i) in
         let gfx = gfx_explosion game gfx Gfx.YellowStar pow x y ya.(i) in
         let gfx = gfx_explosion game gfx Gfx.RedStar pow x y ra.(i) in
         let gfx = gfx_explosion game gfx Gfx.PurpleStar pow x y pa.(i) in
         gfx, i + 1)
      (gfx, 0) puyos
  in
  gfx

let gfx_chain game gfx count puyos =
  if count > 1 then begin
    let x, y =
      List.fold_left
        (fun (ax, ay) (x, y) -> ax + x, ay + y)
        (0, 0)
        puyos
    in
    let len = float_of_int (List.length puyos) in
    let x = float_of_int x /. len in
    let y = float_of_int y /. len in
    Gfx.add gfx (Gfx.Chain (count, x, y)) (game.now + 80)
  end else
    gfx

(* TODO (maybe): remove garbage uniformly *)
let rec remove_incoming_garbage g = function
  | [] -> g, []
  | (p, x) :: r ->
      if x > g then
        0, (p, x - g) :: r
      else if x = g then
        0, r
      else
        remove_incoming_garbage (g - x) r

let apply_garbage_acceleration game g =
  if game.now < game.speed.sp_garbage_initial
    || game.speed.sp_garbage_acceleration_delay <= 0 then
      g
  else
    (100
     + (game.now - game.speed.sp_garbage_initial)
     / game.speed.sp_garbage_acceleration_delay)
    * g
    / 100

let think_popping game ps =
  if game.now >= ps.pop_end then
    let offsets =
      if List.exists (fun (_, g) -> g > 0) game.garbage_incoming
        || game.garbage_ready > 0 then
          game.offsets + 1
      else
        game.offsets
    in
    let offsets = min 7 offsets in
    let fury = check_and_start_fury offsets game in
    let field = pop_puyos game.field ps.pop_puyos in
    let add_score = ps.pop_score_base * ps.pop_score_mult in
    let screen_cleared = is_empty_field field in
    let add_score =
      if screen_cleared then add_score * 2 + 1000
      else add_score
    in
    let gfx =
      if screen_cleared then gfx_clear_screen game
      else game.gfx
    in
    let gfx = gfx_pop game gfx ps.pop_puyos add_score in
    let gfx = gfx_chain game gfx ps.pop_chain ps.pop_puyos in
    let garbage = ceil_div add_score score_per_garbage in
    let garbage = apply_garbage_acceleration game garbage in
    let garbage, garbage_ready =
      if game.garbage_ready >= garbage then 0, game.garbage_ready - garbage
      else garbage - game.garbage_ready, 0
    in
    let garbage, garbage_incoming =
      remove_incoming_garbage garbage game.garbage_incoming
    in
    check_and_start_chain
      { game with
	  field = field;
	  score = game.score + add_score;
	  garbage_incoming = garbage_incoming;
	  garbage_ready = garbage_ready;
          garbage_sent = game.garbage_sent + garbage;
          offsets = offsets;
          fury = fury;
          gfx = gfx }
  else game

let think_game_over game gos =
  if gos.go_end > game.now then
    let gos = {
      gos with
        go_speed = gos.go_speed + game.speed.sp_gravity;
        go_y = gos.go_y + gos.go_speed;
    } in
    { game with state = GameOver gos }
  else
    game

let rotate rotate_fun game is =
  try
    let b = is.inc_block and x = is.inc_x and y = is.inc_y in
    let b1 = rotate_fun b in
    let b2 = rotate_fun b1 in
    let ytop = y / smooth_factor * smooth_factor in
    let ytop = if ytop = y then ytop - smooth_factor else ytop in
    let b, x, y =
      List.find
        (fun (b, x, y) -> not (Block.collision b x (unsmooth_y y) game.field))
        [
          b1, x, y;
          b1, x, ytop;
          b1, x - 1, y;
          b1, x + 1, y;
          b2, x, y;
          b2, x, ytop;
        ]
    in
    let is = { is with inc_block = b; inc_x = x; inc_y = y } in
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
  (*let gfx = gfx_explosion game game.gfx Gfx.GreenStar 1. 120 300 10 in
  let gfx = gfx_explosion game gfx Gfx.YellowStar 1.2 120 300 5 in
  let gfx = gfx_explosion game gfx Gfx.RedStar 1.4 120 300 2 in
  let gfx = gfx_explosion game gfx Gfx.PurpleStar 1.6 120 300 1 in
  { game with
      offsets = 6;
      garbage_ready = game.garbage_ready + 1;
      gfx = gfx }*)
  (* stairs, to test puyo rotations *)
  (*let field =
    List.fold_left
      (fun f x ->
         List.fold_left
           (fun f y ->
              if (*y < x ||*) y < (5 - x) then f else
                Matrix.set f x (y + 8) (Cell.make Puyo.gray))
           f
           [ 0; 1; 2; 3; 4; 5 ])
      game.field
      [ 0; 1; 2; 3; 4 ]
  in
  { game with field = field }*)
  game

let act_incoming game is = function
  | RLeft -> rotate Block.rotate_left game is
  | RRight -> rotate Block.rotate_right game is
  | InstaFall ->
      if game.speed.sp_allow_insta_fall then
        insta_fall game is
      else
        game
  | Debug -> debug game
  | _ -> game

let act game input =
  match input with
    | SendGarbage (pid, count) ->
(*        Printf.printf "SendGarbage (%d, %d)\n%!" pid count;*)
        let a, (_, g), b =
          try split_when (fun (i, _) -> i = pid) game.garbage_incoming
          with Not_found -> [], (pid, 0), game.garbage_incoming
        in
        { game with garbage_incoming = a @ ((pid, g + count) :: b) }
    | FinishGarbage pid ->
(*        Printf.printf "FinishGarbage %d\n%!" pid;*)
        let a, (_, g), b =
          try split_when (fun (i, _) -> i = pid) game.garbage_incoming
          with Not_found -> [], (pid, 0), game.garbage_incoming
        in
        { game with
            garbage_incoming = a @ b;
            garbage_ready = game.garbage_ready + g }
    | MDown ->
        { game with fast_fall = true }
    | MDownRelease ->
        { game with fast_fall = false }
    | MLeft ->
        { game with left_last_press = 0 }
    | MRight ->
        { game with right_last_press = 0 }
    | MLeftRelease ->
        { game with left_last_press = -1 }
    | MRightRelease ->
        { game with right_last_press = -1 }
    | _ ->
        match game.state with
          | Starting _
          | Inserting _
          | Falling _
          | Popping _
          | GameOver _ -> game
          | Incoming is -> act_incoming game is input

let think_fury game =
  match game.fury with
    | FNone -> game
    | FInitial t ->
        let delay = game.speed.sp_fury_initial_delay in
        if t <= game.now then
          { game with fury = FDown (game.now + delay, delay) }
        else
          game
    | FDown (t, delay) ->
        if t <= game.now then
          if game.offsets <= 1 then
            { game with
                offsets = 0;
                fury = FNone }
          else
            let newdelay = delay - game.speed.sp_fury_acceleration in
            let newdelay = max newdelay game.speed.sp_fury_minimum_delay in
            { game with
                fury = FDown (game.now + delay, newdelay);
                offsets = game.offsets - 1 }
        else
          game

let think_starting game ss =
  if game.now >= ss.s_next then begin
    let cd = ss.s_countdown - 1 in
    if cd <= 0 then
      start_incoming game
    else
      let ss = {
        s_countdown = cd;
        s_next = ss.s_next + ready_set_go_delay;
      } in
      { game with state = Starting ss }
  end else
    game

let think game =
  let game =
    { game with
        now = game.now + 1;
        gfx = Gfx.remove game.gfx game.now }
  in
  let game = think_fury game in
  match game.state with
    | Starting ss -> think_starting game ss
    | Incoming is -> think_incoming game is
    | Inserting is -> think_inserting game is
    | Falling fs -> think_falling game fs
    | Popping ps -> think_popping game ps
    | GameOver gos -> think_game_over game gos

let think_frame game actions =
  let game = List.fold_left act game actions in
  think game

let normal_speed =
  {
    sp_fall_absorb = smooth_factor;
    sp_fall = 10;
    sp_fall_fast = 200;
    sp_insert_delay = 40;
    sp_gravity = 6;
    sp_pop_delay = 80;
    sp_fury_initial_delay = 200;
    sp_fury_acceleration = 10;
    sp_fury_minimum_delay = 50;
    sp_fury_initial = 500;
    sp_fury_gravity = 20;
    sp_fury_pop_delay = 25;
    sp_garbage_initial = 0;
    sp_garbage_acceleration_delay = 0;
    sp_allow_insta_fall = true;
  }

let start ?(generator = Generator.nice) ?rand () =
  let rand =
    match rand with
      | None -> Rand.self_init ()
      | Some rand -> rand
  in
  let rand, generator, block1 = Generator.next generator rand in
  let rand, generator, block2 = Generator.next generator rand in
  {
    now = -ready_set_go_delay * 2;
    field = Matrix.make 6 (12 + invisible_lines) Cell.empty;
    rand = rand;
    generator = generator;
    state = Starting {
      s_countdown = 2;
      s_next = -ready_set_go_delay;
    };
    score = 0;
    chain = 1;
    speed = normal_speed;
    next_blocks = [ block1; block2 ];
    garbage_incoming = [];
    garbage_ready = 0;
    garbage_sent = 0;
    garbage_finished = false;
    garbage_protection = false;
    garbage_position = -1;
    offsets = 0;
    fury = FNone;
    gfx = Gfx.empty;

    left_last_press = -1;
    right_last_press = -1;
    fast_fall = false;
  }

let start_multiplayer ?generator rand =
  let game = start ?generator ~rand () in
  { game with
      speed = {
        game.speed with
          sp_garbage_initial = 6000; (* 1 minute *)
          sp_garbage_acceleration_delay = 60; (* +100% per minute *)
          sp_allow_insta_fall = false;
(*	  sp_gravity = 7;
	  sp_pop_delay = 70;
	  sp_fury_pop_delay = 25;*)
      }
  }

let start_sandbox ?generator speed () =
  let game = start ?generator () in
  let speed = match speed with
    | `None ->
	{
          game.speed with
            sp_fall = 0;
(*            sp_fall_fast = 100;*)
	}
    | `VerySlow ->
	{
          game.speed with
	    sp_fall = game.speed.sp_fall / 5;
(*            sp_fall_fast = 100;*)
	}
    | `Slow ->
	{
          game.speed with
	    sp_fall = game.speed.sp_fall / 2;
(*            sp_fall_fast = 100;*)
	}
    | `Normal ->
	game.speed
    | `Fast ->
	{
          game.speed with
	    sp_fall = game.speed.sp_fall * 3;
	}
    | `VeryFast ->
	{
          game.speed with
	    sp_fall = game.speed.sp_fall * 10;
	}
  in
  { game with speed = speed }
