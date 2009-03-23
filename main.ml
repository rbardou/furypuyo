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

(** Entry point *)

open Misc

let show_version () =
  print_endline Version.string;
  exit 0

let speclist = Arg.align [
  "-version", Arg.Unit show_version, " Show version and exit"
]
let usage_msg = "furypuyo [options]"
let anon_fun x = raise (Arg.Bad ("unknown option: `"^x^"'"))
let () = Arg.parse speclist anon_fun usage_msg

module Reader = IO.MakeReader(Action)

module Score = struct
  type t = int
  let compare = compare
end

module HighScores = Highscores.Make(Score)

let () =
  Sdlwm.set_caption ~title: "Fury Puyo" ~icon: "Fury Puyo";
  Config.init ~var: "FURYPUYOCONF" "~/.furypuyo";
  Reader.key_down Sdlkey.KEY_ESCAPE Action.Escape;
  Reader.key_auto 100 30 Sdlkey.KEY_LEFT Action.MLeft;
  Reader.key_auto 100 30 Sdlkey.KEY_RIGHT Action.MRight;
  Reader.key_down Sdlkey.KEY_UP Action.RRight;
  Reader.key_down Sdlkey.KEY_RCTRL Action.RLeft;
  Reader.key_down Sdlkey.KEY_LCTRL Action.RLeft;
  Reader.key_down Sdlkey.KEY_LALT Action.RRight;
  Reader.key_down Sdlkey.KEY_KP0 Action.RRight;
  Reader.key_down Sdlkey.KEY_SPACE Action.InstaFall;
  Reader.key_down Sdlkey.KEY_d Action.Debug;
  Reader.key_down Sdlkey.KEY_DOWN Action.MDown;
  Reader.key_up Sdlkey.KEY_DOWN Action.MDownRelease

let config =
  Config.load "furypuyo.cfg" "Fury Puyo configuration file"

let player_name =
  Config.string config "PLAYERNAME" "Player name"
    (try Sys.getenv "USER" with Not_found ->
       try Sys.getenv "LOGNAME" with Not_found ->
         "Fury Puyo")

let high_scores = ref (HighScores.load 10 "single_player.scores")

let draw = ref true

let quit () =
  Config.save config;
  IO.close ()

let game_finished game =
  match game.Game.state with
    | Game.GameOver s -> s.Game.go_end <= game.Game.now
    | _ -> false

let game_over game =
  match game.Game.state with
    | Game.GameOver _ -> true
    | _ -> false

let rec loop game cpu: unit =
  let actions = Reader.read () in
  if not (game_over game) && List.mem Action.Escape actions then
    pause game cpu
  else if game_finished game then
    enter_score game.Game.score
  else
    let game = List.fold_left Game.act game actions in
    let game = Game.think game in
    let game, cpu = Cpu.think game cpu in
    if !draw then Draw.draw game;
    draw := IO.frame_delay 10;
    loop game cpu

and pause game cpu: unit =
  Draw.draw_empty ();
  let choice =
    Menu.string_choices ~default: `Continue [
      "CONTINUE", `Continue;
      "RESTART", `Restart;
      "MAIN MENU", `MainMenu;
      "QUIT", `Quit;
    ]
  in
  match choice with
    | `Continue ->
        IO.timer_start ();
        loop game cpu
    | `Restart ->
        single_player_game ()
    | `MainMenu ->
        main_menu ()
    | `Quit ->
        quit ()

and single_player_game (): unit =
  let game = Game.start () in
  let cpu = Cpu.start in
  IO.timer_start ();
  loop game cpu

and main_menu (): unit =
  Draw.draw_empty ();
  let choice =
    Menu.string_choices [
      "SINGLE PLAYER", `Single;
      "HIGH SCORES", `HighScores;
      "QUIT", `Quit;
    ]
  in
  match choice with
    | `Single ->
        single_player_game ()
    | `HighScores ->
        show_high_scores ();
        main_menu ()
    | `Quit ->
        quit ()

and game_over_menu (): unit =
  let choice =
    Menu.string_choices [
      "PLAY AGAIN", `Again;
      "MAIN MENU", `MainMenu;
      "QUIT", `Quit;
    ]
  in
  match choice with
    | `Again ->
        single_player_game ()
    | `MainMenu ->
        main_menu ()
    | `Quit ->
        quit ()

and show_high_scores ?focus (): unit =
  let top = HighScores.top ~plimit: 1 ~size: 10 !high_scores in
  let top =
    list_mapi
      (fun i (name, score) -> Printf.sprintf "%2d%9d  %s" (i + 1) score name)
      top
  in
  let all = HighScores.all_players !high_scores in
  let all =
    List.map
      (fun (name, scores) ->
         let scores =
           list_mapi
             (fun i score -> Printf.sprintf "%2d%9d" (i + 1) score)
             scores
         in
         name, scores)
      all
  in
  let before, after =
    match focus with
      | None -> [], all
      | Some focus ->
          try
            let before, focus, after =
              split_when (fun (name, _) -> name = focus) all in
            focus :: after, before
          with Not_found ->
            [], all
  in
  let pages = before @ [ "TOP PLAYERS", top ] @ after in
  Draw.draw_empty ();
  Menu.show_high_scores pages

and enter_score score: unit =
  let background = IO.Sprite.screenshot () in
  let name =
    Menu.input_string
      ~default: (Config.get player_name)
      "ENTER YOUR NAME:"
  in
  let scores, changed = HighScores.add !high_scores name score in
  high_scores := scores;
  HighScores.save !high_scores;
  if changed then show_high_scores ~focus: name ();
  IO.Sprite.draw background 0 0;
  game_over_menu ()

let () =
  main_menu ()
