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

open Misc
open Sprites

module MenuAction = struct
  type t = Up | Down | Return | Escape | Left | Right
end
module MenuReader = IO.MakeReader(MenuAction)

open MenuAction

let () =
  MenuReader.key_auto 500 100 Sdlkey.KEY_UP Up;
  MenuReader.key_auto 500 100 Sdlkey.KEY_DOWN Down;
  MenuReader.key_auto 500 100 Sdlkey.KEY_LEFT Left;
  MenuReader.key_auto 500 100 Sdlkey.KEY_RIGHT Right;
  MenuReader.key_down Sdlkey.KEY_RETURN Return;
  MenuReader.key_down Sdlkey.KEY_ESCAPE Escape

let sprite_puyo = IO.Sprite.align sprite_puyo_red IO.Center

let string_choices ?default choices =
  let choices = Array.of_list choices in
  MenuReader.reset ();
  let result = ref (fun () -> assert false) in
  let return x =
    result := (fun () -> x);
    raise Exit
  in
  let background = IO.Sprite.screenshot () in
  let choice = ref 0 in
  let count = Array.length choices in
  IO.timer_start ();
  let choice_y i = (i + 1) * screen_height / (count + 1) in
  let choice_x = screen_width / 2 in
  let choice_radius =
    Array.init count
      (fun i ->
         fst (IO.Text.size font (fst choices.(i))) / 2 +
           IO.Sprite.width sprite_puyo)
  in
  let puyo_x = ref (float_of_int choice_radius.(!choice)) in
  let puyo_y = ref (float_of_int (choice_y !choice)) in
  try
    while true do
      if IO.frame_delay 10 then begin
        IO.Sprite.draw background 0 0;
        Array.iteri
          (fun i (str, _) ->
             let y = choice_y i in
             IO.Text.write font ~align: IO.Center choice_x y str)
          choices;

        IO.Sprite.draw sprite_puyo (choice_x - int_of_float !puyo_x)
          (int_of_float !puyo_y);
        IO.Sprite.draw sprite_puyo (choice_x + int_of_float !puyo_x)
          (int_of_float !puyo_y);
      
        IO.update ()
      end;

      List.iter
        (function
           | Up | Left ->
               decr choice;
               if !choice < 0 then choice := count - 1
           | Down | Right ->
               incr choice;
               if !choice >= count then choice := 0
           | Return ->
               return (snd (choices.(!choice)))
           | Escape ->
               match default with
                 | None -> ()
                 | Some d -> return d)
        (MenuReader.read ());

      puyo_y := !puyo_y +. (float_of_int (choice_y !choice) -. !puyo_y) /. 10.;
      puyo_x :=
        !puyo_x +. (float_of_int choice_radius.(!choice) -. !puyo_x) /. 10.;
    done;
    assert false
  with Exit ->
    !result ()

module InputStringAction = struct
  type t =
    | Return
    | BackSpace
    | Char of char
end
module InputStringReader = IO.MakeReader(InputStringAction)

open InputStringAction

let () =
  InputStringReader.key_auto 500 50 Sdlkey.KEY_a (Char 'a');
  InputStringReader.key_auto 500 50 Sdlkey.KEY_b (Char 'b');
  InputStringReader.key_auto 500 50 Sdlkey.KEY_c (Char 'c');
  InputStringReader.key_auto 500 50 Sdlkey.KEY_d (Char 'd');
  InputStringReader.key_auto 500 50 Sdlkey.KEY_e (Char 'e');
  InputStringReader.key_auto 500 50 Sdlkey.KEY_f (Char 'f');
  InputStringReader.key_auto 500 50 Sdlkey.KEY_g (Char 'g');
  InputStringReader.key_auto 500 50 Sdlkey.KEY_h (Char 'h');
  InputStringReader.key_auto 500 50 Sdlkey.KEY_i (Char 'i');
  InputStringReader.key_auto 500 50 Sdlkey.KEY_j (Char 'j');
  InputStringReader.key_auto 500 50 Sdlkey.KEY_k (Char 'k');
  InputStringReader.key_auto 500 50 Sdlkey.KEY_l (Char 'l');
  InputStringReader.key_auto 500 50 Sdlkey.KEY_m (Char 'm');
  InputStringReader.key_auto 500 50 Sdlkey.KEY_n (Char 'n');
  InputStringReader.key_auto 500 50 Sdlkey.KEY_o (Char 'o');
  InputStringReader.key_auto 500 50 Sdlkey.KEY_p (Char 'p');
  InputStringReader.key_auto 500 50 Sdlkey.KEY_q (Char 'q');
  InputStringReader.key_auto 500 50 Sdlkey.KEY_r (Char 'r');
  InputStringReader.key_auto 500 50 Sdlkey.KEY_s (Char 's');
  InputStringReader.key_auto 500 50 Sdlkey.KEY_t (Char 't');
  InputStringReader.key_auto 500 50 Sdlkey.KEY_u (Char 'u');
  InputStringReader.key_auto 500 50 Sdlkey.KEY_v (Char 'v');
  InputStringReader.key_auto 500 50 Sdlkey.KEY_w (Char 'w');
  InputStringReader.key_auto 500 50 Sdlkey.KEY_x (Char 'x');
  InputStringReader.key_auto 500 50 Sdlkey.KEY_y (Char 'y');
  InputStringReader.key_auto 500 50 Sdlkey.KEY_z (Char 'z');
  InputStringReader.key_auto 500 50 Sdlkey.KEY_0 (Char '0');
  InputStringReader.key_auto 500 50 Sdlkey.KEY_1 (Char '1');
  InputStringReader.key_auto 500 50 Sdlkey.KEY_2 (Char '2');
  InputStringReader.key_auto 500 50 Sdlkey.KEY_3 (Char '3');
  InputStringReader.key_auto 500 50 Sdlkey.KEY_4 (Char '4');
  InputStringReader.key_auto 500 50 Sdlkey.KEY_5 (Char '5');
  InputStringReader.key_auto 500 50 Sdlkey.KEY_6 (Char '6');
  InputStringReader.key_auto 500 50 Sdlkey.KEY_7 (Char '7');
  InputStringReader.key_auto 500 50 Sdlkey.KEY_8 (Char '8');
  InputStringReader.key_auto 500 50 Sdlkey.KEY_9 (Char '9');
  InputStringReader.key_auto 500 50 Sdlkey.KEY_SPACE (Char ' ');
  InputStringReader.key_auto 500 50 Sdlkey.KEY_BACKSPACE BackSpace;
  InputStringReader.key_down Sdlkey.KEY_RETURN Return

let input_string ?(default = "") query =
  let result = ref [] in
  for i = 0 to String.length default - 1 do
    result := default.[i] :: !result;
  done;
  let resultstr = ref "" in
  let update_resultstr () =
    let len = List.length !result in
    let res = String.create len in
    let rec fill n = function
      | [] -> ()
      | c :: rem ->
          let n = n - 1 in
          res.[n] <- c;
          fill n rem
    in
    fill len !result;
    resultstr := res
  in
  update_resultstr ();
  let background = IO.Sprite.screenshot () in
  let query_x = screen_width / 2 in
  let query_y = screen_height / 2 - 50 in
  let input_x = screen_width / 2 in
  let input_y = screen_height / 2 + 50 in
  let input_align =
    IO.Custom (0.5, IO.Sprite.width sprite_puyo / 2, 0.5, 0) in
  let now = ref 0 in
  InputStringReader.reset ();
  try
    while true do
      if IO.frame_delay 10 then begin
        IO.Sprite.draw background 0 0;

        IO.Text.write font ~align: IO.Center query_x query_y query;

        let w, _ = IO.Text.size font (String.uppercase !resultstr) in
        IO.Text.write font ~align: input_align input_x input_y
          (String.uppercase !resultstr);

        if !now / 50 mod 2 = 0 then
          IO.Sprite.draw sprite_puyo (input_x + w / 2) input_y;

        IO.update ()
      end;

      List.iter
        (function
           | Char c ->
               result := c :: !result;
               update_resultstr ()
           | BackSpace ->
               begin match !result with
                 | [] -> ()
                 | _ :: rem -> result := rem
               end;
               update_resultstr ()
           | Return ->
               raise Exit)
        (InputStringReader.read ());

      incr now;
    done;
    assert false
  with Exit ->
    !resultstr

open MenuAction

let show_high_scores pages =
  let pages = Array.of_list pages in
  let count = Array.length pages in
  let page = ref 0 in
  let background = IO.Sprite.screenshot () in
  let title_x = screen_width / 2 in
  let title_y = 50 in
  let scores_x = 20 in
  let scores_y = 150 in
  let scores_d = 35 in
  let nothing_x = screen_width / 2 in
  let nothing_y = screen_height / 2 in
  MenuReader.reset ();
  try
    while true do
      let title, lines = pages.(!page) in

      if IO.frame_delay 10 then begin
        IO.Sprite.draw background 0 0;

        let title = String.uppercase title in
        IO.Text.write font ~align: IO.Top title_x title_y title;

        if lines = [] then
          IO.Text.write font ~align: IO.Center nothing_x nothing_y
            "NO SCORE YET"
        else
          list_iteri
            (fun i line ->
               let line = String.uppercase line in
               IO.Text.write font scores_x (scores_y + scores_d * i) line)
            lines;

        IO.update ()
      end;

      List.iter
        (function
           | Up | Left ->
               decr page;
               if !page < 0 then page := count - 1
           | Down | Right ->
               incr page;
               if !page >= count then page := 0
           | Return | Escape ->
               raise Exit)
        (MenuReader.read ());
    done;
    assert false
  with Exit ->
    ()
