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

(** Draw game state on the screen *)

open Gfx
open Block
open Game
open IO
open Puyo
open Cell
open Sprites

let cellw = 40
let cellh = 40
let score_y = 5
let score_x = 20
let score_plus_y = 30
let score_plus_x = 360
let score_plus_align = TopRight
let garbage_x = 20
let garbage_y = 60
let field_x = 20
let field_y = 100
let next_block1_x = 280
let next_block1_y = 60
let next_block2_x = 280
let next_block2_y = 160
let timer_x = 270
let timer_y = 555
let ready_set_go_x = field_x + cellw * 3
let ready_set_go_y = field_y + cellh * 4

let offsets_x = cellw*6+55
let offsets_y = next_block2_y+3*cellh
let offset_scale_x = 4
let offset_scale_y = 25
let ox7 = offsets_x + offset_scale_x * 1
let ox6 = offsets_x + offset_scale_x * 4
let ox5 = offsets_x + offset_scale_x * 5
let ox4 = offsets_x + offset_scale_x * 4
let ox3 = offsets_x + offset_scale_x * 2
let ox2 = offsets_x + offset_scale_x * 0
let ox1 = offsets_x + offset_scale_x * 1
let oy7 = offsets_y + offset_scale_y * 0
let oy6 = offsets_y + offset_scale_y * 1
let oy5 = offsets_y + offset_scale_y * 2
let oy4 = offsets_y + offset_scale_y * 3
let oy3 = offsets_y + offset_scale_y * 4
let oy2 = offsets_y + offset_scale_y * 5
let oy1 = offsets_y + offset_scale_y * 6

let general_offset_x = ref 0
let general_offset_y = ref 0
let second_x = 390
let second_y = 0

module Sprite = struct
  include Sprite
  let draw a x y = draw a (x + !general_offset_x) (y + !general_offset_y)
end

module Text = struct
  include Text
  let write a ?align x y =
    write a ?align (x + !general_offset_x) (y + !general_offset_y)
end

let sprite_of_puyo p =
  match p.color with
    | Red -> sprite_puyo_red
    | Green -> sprite_puyo_green
    | Blue -> sprite_puyo_blue
    | Yellow -> sprite_puyo_yellow
    | Gray -> sprite_puyo_gray

let draw_puyo p =
  Sprite.draw (sprite_of_puyo p)

let field_puyo_x x = cellw * x + field_x
let field_puyo_y y = cellh * (y - Game.invisible_lines) + field_y

let draw_field_puyo p x y y_offset =
  Sprite.draw
    (sprite_of_puyo p)
    (field_puyo_x x)
    (field_puyo_y y + cellh * y_offset / Game.smooth_factor)

let block_puyos = function
  | List0 l | List1 l | List2 l -> l
  | Quad (c, _) ->
      let p = Puyo.make c in
      [ 0, 0, p; 0, 1, p; 1, 0, p; 1, 1, p ]

let draw_block block x y =
  List.iter
    (fun (x', y', p) -> draw_puyo p (x + cellw * x') (y + cellh * y'))
    (block_puyos block)

let draw_incoming game is =
  let x = field_puyo_x is.inc_x in
  let y = field_puyo_y 0 + cellh * is.inc_y / Game.smooth_factor in
  draw_block is.inc_block x y

let draw_inserting game is =
  let anim_pos =
    float_of_int (is.ins_end - game.now)
    /. float_of_int game.speed.sp_insert_delay
  in
  let shift =
    int_of_float
      (float_of_int cellh *. 0.5
       *. anim_pos *. (anim_pos -. 1.) *. (anim_pos -. 5.))
  in
  let x = field_puyo_x is.ins_x in
  let y = field_puyo_y is.ins_y + shift in
  draw_block is.ins_block x y

let draw_falling game fs =
  let draw_puyo p x y =
    draw_puyo p
      (field_puyo_x x)
      (field_puyo_y y + cellh * fs.f_y / Game.smooth_factor)
  in
  List.iter (fun (x, y, p) -> draw_puyo p x y) fs.f_puyos

let actually_draw_garbage count =
  let c720 = count / 720 in
  let count = count mod 720 in
  let c360 = count / 360 in
  let count = count mod 360 in
  let c180 = count / 180 in
  let count = count mod 180 in
  let c30 = count / 30 in
  let count = count mod 30 in
  let c6 = count / 6 in
  let count = count mod 6 in
  let list = [
    c720, garbage720;
    c360, garbage360;
    c180, garbage180;
    c30, garbage30;
    c6, garbage6;
    count, garbage1;
  ] in
  let draw x s = Sprite.draw s (x * cellw + garbage_x) garbage_y in
  let rec go x l =
    if x < 6 then match l with
      | [] -> ()
      | (0, sprite) :: rem ->
	  go x rem
      | (n, sprite) :: rem ->
	  draw x sprite;
	  go (x + 1) ((n - 1, sprite) :: rem)
  in
  go 0 list

let draw_garbage game blit =
  let count =
    List.fold_left (fun a (_, g) -> a + g) 0 game.garbage_incoming
    + game.garbage_ready
  in
  if game.garbage_ready <= 0 || blit then
    actually_draw_garbage count

let draw_offsets n fury blit =
  let draw m x y =
    let sprite = if fury && blit then offset_fury else offset in
    if n >= m then Sprite.draw sprite x y
  in
  draw 7 ox7 oy7;
  draw 6 ox6 oy6;
  draw 5 ox5 oy5;
  draw 4 ox4 oy4;
  draw 3 ox3 oy3;
  draw 2 ox2 oy2;
  draw 1 ox1 oy1

let gfx = function
  | ClearScreen ->
      Sprite.draw all_clear (field_x + 3 * cellw) (field_y + 5 * cellh)
  | Particle p ->
      let x = field_puyo_x p.cx + cellw / 2 in
      let y = field_puyo_y p.cy + cellh / 2 in
      let sprite = match p.sprite with
        | GreenStar -> green_star
        | YellowStar -> yellow_star
        | RedStar -> red_star
        | PurpleStar -> purple_star
      in
      Sprite.draw sprite (x + int_of_float p.x) (y + int_of_float p.y);
      p.x <- p.x +. p.vx;
      p.y <- p.y +. p.vy;
      p.vx <- p.vx +. p.ax;
      p.vy <- p.vy +. p.ay
  | Chain (c, x, y) ->
      let s = chain c in
      let x =
        field_puyo_x 0 + int_of_float ((x +. 0.5) *. float_of_int cellw) in
      let y =
        field_puyo_y 0 + int_of_float ((y +. 0.5) *. float_of_int cellh) in
      let x = max x (field_x + Sprite.width s / 2) in
      let x = min x (field_x + 6 * cellw - Sprite.width s / 2) in
      Sprite.draw s x y

let draw_timer now =
  let now = max 0 now in
  let s = now / 100 in
  let m = s / 60 in
  let s = s mod 60 in
  Text.write font timer_x timer_y (Printf.sprintf "%02d:%02d" m s)

let draw_ready_set_go now =
  let draw x = Sprite.draw x ready_set_go_x ready_set_go_y in
  if now < -Game.ready_set_go_delay then
    draw sprite_ready
  else if now < 0 then
    draw sprite_set
  else if now < Game.ready_set_go_delay then
    draw sprite_go

let draw_empty_1 () =
  general_offset_x := 0;
  general_offset_y := 0;
  Sprite.draw background 0 0;
  Sprite.draw foreground 0 0

let draw_empty_2 () =
  general_offset_x := second_x;
  general_offset_y := second_y;
  Sprite.draw background 0 0;
  Sprite.draw foreground 0 0

let draw_empty () =
  draw_empty_1 ();
  draw_empty_2 ()

let draw_game_at ?(name = "") game game_x game_y =
  general_offset_x := game_x;
  general_offset_y := game_y;
  let blit = game.now / 2 mod 2 = 0 in
  Sprite.draw background 0 0;
  let y_offset = ref 0 in
  begin match game.state with
    | Incoming is -> draw_incoming game is
    | Inserting is -> draw_inserting game is
    | Falling fs -> draw_falling game fs
    | GameOver gos -> y_offset := gos.go_y
    | _ -> ()
  end;
  let hidden = match game.state with
    | Popping ps ->
        if blit then ps.pop_puyos else []
    | _ -> []
  in
  for x = 0 to Matrix.width game.field - 1 do
    for y = 0 to Matrix.height game.field - 1 do
      match (Matrix.get game.field x y).puyo with
        | None -> ()
        | Some puyo ->
            if not (List.mem (x, y) hidden) then
              draw_field_puyo puyo x y !y_offset
    done;
  done;
  begin match game.next_blocks with
    | b1 :: b2 :: _ ->
        draw_block b1 next_block1_x next_block1_y;
        draw_block b2 next_block2_x next_block2_y
    | _ -> ()
  end;
  draw_offsets game.offsets (game.fury <> FNone) blit;
  Sprite.draw foreground 0 0;
  draw_garbage game blit;
  Text.write font score_x score_y (string_of_int game.score);
  begin match name, game.state with
    | "", Popping ps ->
        Text.write font ~align: score_plus_align score_plus_x score_plus_y
          (Printf.sprintf "+ %d x %d" ps.pop_score_base ps.pop_score_mult);
    | _ ->
        Text.write font ~align: score_plus_align score_plus_x score_plus_y
          (String.uppercase name)
  end;
  let now = match game.state with
    | GameOver s -> s.go_end
    | _ -> game.now
  in
  draw_ready_set_go now;
  draw_timer now;
  Gfx.iter gfx game.gfx;
  update ()

let draw game =
  draw_empty_2 ();
  draw_game_at game 0 0

let draw_multiplayer game1 game2 =
  draw_game_at game1 0 0;
  match game2 with
    | None -> ()
    | Some (name, game2) ->
        draw_game_at ~name game2 second_x second_y
