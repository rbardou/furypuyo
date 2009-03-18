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

open IO

let screen_width = 390
let screen_height = 600

let () = IO.init screen_width screen_height

let load_alpha = Sprite.load ~transparency: `ALPHA
let sprite_puyo_red = load_alpha "data/red40.png"
let sprite_puyo_green = load_alpha "data/green40.png"
let sprite_puyo_blue = load_alpha "data/blue40.png"
let sprite_puyo_yellow = load_alpha "data/yellow40.png"
let sprite_puyo_gray = load_alpha "data/gray40.png"
let foreground = Sprite.load "data/foreground.png"
let background = Sprite.load "data/background.png"
let garbage1 = Sprite.load "data/garbage1.png"
let garbage6 = Sprite.load "data/garbage6.png"
let garbage30 = Sprite.load "data/garbage30.png"
let offset = Sprite.load "data/offset.png"
let offset_fury = Sprite.load "data/offsetfury.png"
let green_star = Sprite.load ~transparency: `BLACK "data/greenstar.png"
let yellow_star = Sprite.load ~transparency: `BLACK "data/yellowstar.png"
let red_star = Sprite.load ~transparency: `BLACK "data/redstar.png"
let purple_star = Sprite.load ~transparency: `BLACK "data/purplestar.png"

let char_sprite font size width char =
  let sprite = try
    let char = match char with
      | '0' -> "0"
      | '1' -> "1"
      | '2' -> "2"
      | '3' -> "3"
      | '4' -> "4"
      | '5' -> "5"
      | '6' -> "6"
      | '7' -> "7"
      | '8' -> "8"
      | '9' -> "9"
      | 'A' -> "am"
      | 'B' -> "bm"
      | 'C' -> "cm"
      | 'D' -> "dm"
      | 'E' -> "em"
      | 'F' -> "fm"
      | 'G' -> "gm"
      | 'H' -> "hm"
      | 'I' -> "im"
      | 'J' -> "jm"
      | 'K' -> "km"
      | 'L' -> "lm"
      | 'M' -> "mm"
      | 'N' -> "nm"
      | 'O' -> "om"
      | 'P' -> "pm"
      | 'Q' -> "qm"
      | 'R' -> "rm"
      | 'S' -> "sm"
      | 'T' -> "tm"
      | 'U' -> "um"
      | 'V' -> "vm"
      | 'W' -> "wm"
      | 'X' -> "xm"
      | 'Y' -> "ym"
      | 'Z' -> "zm"
      | '+' -> "plus"
      | ':' -> "colon"
      | 'x' -> "x"
      | _ -> raise Not_found
    in
    let file = Printf.sprintf "%s_%d_%s.png" font size char in
    if Sys.file_exists file then
      Some (Sprite.load ~transparency: `ALPHA file)
    else
      None
  with Not_found ->
    None
  in
  let addw = match char with
    | ' ' -> width
    | _ -> 0
  in
  sprite, addw

let font = Text.make (char_sprite "data/font" 25 19)
