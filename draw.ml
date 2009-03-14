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

let cellw = 40
let cellh = 40
let garbage_x = 20
let garbage_y = 60
let field_x = 20
let field_y = 100
let next_block1_x = 280
let next_block1_y = 60
let next_block2_x = 280
let next_block2_y = 160

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

let () = IO.init 390 600

let load_alpha = Sprite.load ~transparency: `ALPHA
let sprite_puyo_red = load_alpha "data/red40.png"
let sprite_puyo_green = load_alpha "data/green40.png"
let sprite_puyo_blue = load_alpha "data/blue40.png"
let sprite_puyo_yellow = load_alpha "data/yellow40.png"
let sprite_puyo_gray = load_alpha "data/gray40.png"
let foreground = Sprite.load "data/foreground.png"
let background = Sprite.load "data/background.png"
let garbage1 = load_alpha "data/garbage1.png"
let garbage6 = load_alpha "data/garbage6.png"
let garbage30 = load_alpha "data/garbage30.png"
let offset = Sprite.load "data/offset.png"
let offset_fury = Sprite.load "data/offsetfury.png"
let font = Text.load "data/pouyou.ttf" 16

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
let field_puyo_y y = cellh * (y - 2) + field_y

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

let draw_falling game fs =
  let draw_puyo p x y =
    draw_puyo p
      (field_puyo_x x)
      (field_puyo_y y + cellh * fs.f_y / Game.smooth_factor)
  in
  List.iter (fun (x, y, p) -> draw_puyo p x y) fs.f_puyos

let draw_garbage count =
  let c30 = count / 30 in
  let count = count mod 30 in
  let c6 = count / 6 in
  let count = count mod 6 in
  let list = [ c30, garbage30; c6, garbage6; count, garbage1 ] in
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
      Text.write font ~align: Center ~color: Sdlvideo.red
        (3*cellw) (field_y + 5*cellh) "Clear Screen!"

let draw game =
  let blit = game.now / 2 mod 2 = 0 in
  Sprite.draw background 0 0;
  let y_offset = ref 0 in
  begin match game.state with
    | Incoming is -> draw_incoming game is
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
  draw_garbage (game.garbage_incoming + game.garbage_ready);
  Text.write font ~color: Sdlvideo.red 5 1 (string_of_int game.score);
  begin match game.state with
    | Popping ps ->
        Text.write font ~color: Sdlvideo.red 5 14
          (Printf.sprintf "+ %d x %d" ps.pop_score_base ps.pop_score_mult);
    | _ -> ()
  end;
  Gfx.iter gfx game.gfx;
  update ()
