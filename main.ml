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
open Common

let () =
  Sdlwm.set_caption ~title: "Fury Puyo" ~icon: "Fury Puyo";
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
  Reader.key_up Sdlkey.KEY_DOWN Action.MDownRelease;
  IO.on_quit on_quit

let high_scores_file = "single_player.scores"
let high_scores = ref (HighScores.load high_scores_file 10)

let draw = ref true

(* sandbox configuration *)
let sandbox_speed = ref `None
let sandbox_dropset = ref `Nice

let rec single_player_loop game cpu replay: unit =
  let actions = Reader.read () in
  if not (game_over game) && List.mem Action.Escape actions then
    pause game cpu replay
  else if game_finished game then
    enter_score game.Game.score replay
  else begin
    let cpu_actions, cpu = Cpu.think game cpu in
    let actions = actions @ cpu_actions in
    Replay.frame replay actions;
    let game = Game.think_frame game actions in
    if !draw then Draw.draw game;
    draw := IO.frame_delay 10;
    single_player_loop game cpu replay
  end

and sandbox_loop game: unit =
  let actions = Reader.read () in
  if game_over game || List.mem Action.Escape actions then
    main_menu ()
  else begin
    let game = Game.think_frame game actions in
    if !draw then Draw.draw game;
    draw := IO.frame_delay 10;
    sandbox_loop game
  end

and replay_loop replay: unit =
  let actions = Reader.read () in
  if List.mem Action.Escape actions then
    replay_pause replay
  else begin
    let game = Replay.next replay in
    if game_finished game then
      main_menu ()
    else begin
      if !draw then Draw.draw game;
      draw := IO.frame_delay 10;
      replay_loop replay
    end
  end

and pause game cpu replay: unit =
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
        single_player_loop game cpu replay
    | `Restart ->
        single_player_game ()
    | `MainMenu ->
        main_menu ()
    | `Quit ->
        quit ()

and replay_pause r: unit =
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
        replay_loop r
    | `Restart ->
        replay r
    | `MainMenu ->
        main_menu ()
    | `Quit ->
        quit ()

and single_player_game (): unit =
  let game = Game.start () in
  let replay = Replay.record game in
  let cpu = Cpu.start in
  IO.timer_start ();
  single_player_loop game cpu replay

and sandbox speed dropset (): unit =
  let generator = match dropset with
    | `Nice -> Generator.nice
    | `Classic -> Generator.classic
  in
  let game = Game.start_sandbox ~generator speed () in
  IO.timer_start ();
  sandbox_loop game

and sandbox_menu (): unit =
  Draw.draw_empty ();
  let speed = ref !sandbox_speed in
  let dropset = ref !sandbox_dropset in
  let speeds = [| `None; `VerySlow; `Slow; `Normal; `Fast; `VeryFast |] in
  let dropsets = [| `Nice; `Classic |] in
  let next r a () =
    let i = array_find ((=) !r) a in
    let i = if i >= Array.length a - 1 then 0 else i + 1 in
    r := a.(i)
  in
  let prev r a () =
    let i = array_find ((=) !r) a in
    let i = if i <= 0 then Array.length a - 1 else i - 1 in
    r := a.(i)
  in
  let print_speed () =
    match !speed with
      | `None -> "NONE"
      | `VerySlow -> "VERY SLOW"
      | `Slow -> "SLOW"
      | `Normal -> "NORMAL"
      | `Fast -> "FAST"
      | `VeryFast -> "VERY FAST"
  in
  let print_dropset () =
    match !dropset with
      | `Nice -> "2223222B22232224"
      | `Classic -> "2222222222222222"
  in
  if Menu.option_menu [
    "SPEED", prev speed speeds, next speed speeds, print_speed;
    "DROPSET", prev dropset dropsets, next dropset dropsets, print_dropset;
  ] then begin
    sandbox_speed := !speed;
    sandbox_dropset := !dropset;
    sandbox !speed !dropset ()
  end else
    main_menu ()

and replay r: unit =
  Replay.play r;
  IO.timer_start ();
  replay_loop r

and replay_file file: unit =
  let ch = open_in file in
  let r = Bin.read (Bin.from_channel ch) Replay.codec in
  close_in ch;
  replay r

and main_menu (): unit =
  Draw.draw_empty ();
  let choice =
    Menu.string_choices [
      "SINGLE PLAYER", `Single;
      "PLAY ONLINE", `PlayOnline;
      "SANDBOX", `Sandbox;
      "HIGH SCORES", `HighScores;
      "QUIT", `Quit;
    ]
  in
  match choice with
    | `Single ->
        single_player_game ()
    | `Sandbox ->
        sandbox_menu ()
    | `PlayOnline ->
        play_online ()
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
      (fun i (name, score) ->
         Printf.sprintf "%2d%9d  %s" (i + 1) (Score.score score) name)
      top
  in
  let all = HighScores.all_players !high_scores in
  let all =
    List.map
      (fun (name, scores) ->
         let scores =
           list_mapi
             (fun i score ->
                Printf.sprintf "%2d%9d" (i + 1) (Score.score score))
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

and enter_score score replay: unit =
  let background = IO.Sprite.screenshot () in
  let name =
    Menu.input_string
      ~default: (Config.get player_name)
      "ENTER YOUR NAME:"
  in
  Config.set player_name name;
  save_replay replay (Printf.sprintf "%s_%d" name score);
  let scores, changed = HighScores.add !high_scores name (Score.make score) in
  high_scores := scores;
  HighScores.save !high_scores high_scores_file;
  if changed then show_high_scores ~focus: name ();
  IO.Sprite.draw background 0 0;
  game_over_menu ()

and play_online (): unit =
  match Online.connection_screen () with
    | None ->
	main_menu ()
    | Some (cx, login) ->
        Online.send_score cx (HighScores.player !high_scores login);
        if Online.menu cx login then
	  main_menu ()
	else
	  quit ()

let () =
  Sdlmouse.show_cursor false;
  match !replay_file_name with
    | "" ->
        main_menu ()
    | file ->
        replay_file file
