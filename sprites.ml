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

(** Sprites of the game *)

open IO
open Common

let screen_width = 390 * 2
let screen_height = 600

let () = IO.init screen_width screen_height

let load_alpha =
  if Config.get alpha then
    Sprite.load ~transparency: `ALPHA
  else
    Sprite.load ~transparency: `BLACK
let load_black = Sprite.load ~transparency: `BLACK

let file =
  let data_directory = Config.get data_directory in
  fun s -> Filename.concat data_directory s

let sprite_puyo_red = load_alpha (file "red40.png")
let sprite_puyo_green = load_alpha (file "green40.png")
let sprite_puyo_blue = load_alpha (file "blue40.png")
let sprite_puyo_yellow = load_alpha (file "yellow40.png")
let sprite_puyo_gray = load_alpha (file "gray40.png")
let foreground = Sprite.load (file "foreground.png")
let background = Sprite.load (file "background.png")
let garbage1 = Sprite.load (file "garbage1.png")
let garbage6 = Sprite.load (file "garbage6.png")
let garbage30 = Sprite.load (file "garbage30.png")
let garbage180 = Sprite.load (file "garbage180.png")
let garbage360 = Sprite.load (file "garbage360.png")
let garbage720 = Sprite.load (file "garbage720.png")
let offset = Sprite.load (file "offset.png")
let offset_fury = Sprite.load (file "offsetfury.png")
let green_star = load_black (file "greenstar.png")
let yellow_star = load_black (file "yellowstar.png")
let red_star = load_black (file "redstar.png")
let purple_star = load_black (file "purplestar.png")
let all_clear = load_alpha ~align: IO.Center (file "allclear.png")
let sprite_ready = load_alpha ~align: IO.Center (file "ready.png")
let sprite_set = load_alpha ~align: IO.Center (file "set.png")
let sprite_go = load_alpha ~align: IO.Center (file "go.png")

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
      | '.' -> "dot"
      | '*' -> "star"
      | 'x' -> "x"
      | '\'' -> "quote"
      | '(' -> "lpar"
      | ')' -> "rpar"
      | '%' -> "percent"
      | ',' -> "comma"
      | _ -> raise Not_found
    in
    let file = Printf.sprintf "%s_%d_%s.png" font size char in
    if Sys.file_exists file then
      Some (load_alpha file)
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

let font = Text.make (char_sprite (file "font") 25 19)

let chain =
  let load_chain i =
    load_alpha ~align: IO.Center (file (Printf.sprintf "chain%d.png" i)) in
  let chain2 = load_chain 2 in
  let chain3 = load_chain 3 in
  let chain4 = load_chain 4 in
  let chain5 = load_chain 5 in
  let chain6 = load_chain 6 in
  let chain7 = load_chain 7 in
  let chain8 = load_chain 8 in
  let chain9 = load_chain 9 in
  let chain10 = load_chain 10 in
  let chain11 = load_chain 11 in
  let chain12 = load_chain 12 in
  let chain13 = load_chain 13 in
  let chain14 = load_chain 14 in
  let chain15 = load_chain 15 in
  let chain16 = load_chain 16 in
  let chain17 = load_chain 17 in
  let chain18 = load_chain 18 in
  let chain19 = load_chain 19 in
  function
    | 2 -> chain2
    | 3 -> chain3
    | 4 -> chain4
    | 5 -> chain5
    | 6 -> chain6
    | 7 -> chain7
    | 8 -> chain8
    | 9 -> chain9
    | 10 -> chain10
    | 11 -> chain11
    | 12 -> chain12
    | 13 -> chain13
    | 14 -> chain14
    | 15 -> chain15
    | 16 -> chain16
    | 17 -> chain17
    | 18 -> chain18
    | 19 -> chain19
    | _ -> purple_star
