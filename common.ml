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

(** Parts which are common to single and multi-player games *)

open Misc

let show_version () =
  print_endline Version.string;
  exit 0

let replay_file_name = ref ""

let speclist = Arg.align [
  "-version", Arg.Unit show_version, " Show version and exit";
  "-replay", Arg.Set_string replay_file_name, "<file> Play a game replay";
]
let usage_msg = "furypuyo [options]"
let anon_fun x = raise (Arg.Bad ("unknown option: `"^x^"'"))
let () = Arg.parse speclist anon_fun usage_msg

let config =
  Config.init ~var: "FURYPUYOCONF" "~/.furypuyo";
  Config.load "furypuyo.cfg" "Fury Puyo configuration file"

let on_quit () =
  Config.save config;
  true

let quit () =
  ignore (on_quit ());
  IO.close ()

(* don't use Sys.executable_name because it won't work with symbolic links *)
let data_directory =
  let ed = Filename.dirname Sys.argv.(0) in
  let ed =
    if Filename.is_relative ed then
      Filename.concat (Sys.getcwd ()) ed
    else
      ed
  in
  let def = Filename.concat ed "data" in
  Config.string config "DATADIR" "Game data directory (sprites, ...)" def

let player_name =
  Config.string config "PLAYERNAME" "Player name"
    (try Unix.getlogin () with Unix.Unix_error _ ->
       try (Unix.getpwuid (Unix.getuid ())).Unix.pw_name
       with Unix.Unix_error _ ->
         try Sys.getenv "USER" with Not_found ->
           try Sys.getenv "LOGNAME" with Not_found ->
             "Fury Puyo")

let server_address =
  Config.string config "SERVERADDRESS" "Server address for online play"
    "localhost"

let server_port =
  Config.int config "SERVERPORT" "Server port for online play" 4269

let player_dropset =
  Config.custom
    (fun s ->
       match String.uppercase s with
         | "CLASSIC" -> Some `Classic
         | "SINGLE" -> Some `Nice
         | _ -> None)
    (function
       | `Classic -> "CLASSIC"
       | `Nice -> "SINGLE")
    config
    "DROPSET"
    "Player dropset (classic or single)"
    `Nice

let alpha =
  Config.bool config "ALPHA" "Enable sprite transparency" true

(*
let default_color_generator =
  Config.custom
    (fun s ->
       match String.uppercase s with
         | "FURY" -> Some `Fury
         | "TGM" -> Some `TGM
         | _ -> None)
    (function
       | `Fury -> "FURY"
       | `TGM -> "TGM")
    config
    "COLORGENERATOR"
    "Color generator (FURY: flat with lots of unicolors, TGM: default)"
    `TGM
*)

module MenuAction = struct
  type t = Up | Down | Return | Escape | Left | Right | PageUp | PageDown
end
module MenuReader = IO.MakeReader(MenuAction)

let game_finished game =
  match game.Game.state with
    | Game.GameOver s -> s.Game.go_end <= game.Game.now
    | _ -> false

let game_over game =
  match game.Game.state with
    | Game.GameOver _ -> true
    | _ -> false

module Reader = IO.MakeReader(Action)

module HighScores = Highscores.Make(Score)

let save_replay replay file =
  let file = new_file_name (Config.filename file) ".replay" in
  let ch = Config.open_out file in
  let buf = Bin.to_channel ch in
  Bin.write buf Replay.codec replay;
  close_out ch

let rec percent_of_handicap = function
  | 0 -> 100
  | i ->
      let p = percent_of_handicap (i - 1) in
      p + p / 5

let print_speed speed () =
  match !speed with
    | `None -> "NONE"
    | `VerySlow -> "VERY SLOW"
    | `Slow -> "SLOW"
    | `Normal -> "NORMAL"
    | `Fast -> "FAST"
    | `VeryFast -> "VERY FAST"

let print_dropset dropset () =
  match !dropset with
    | `Nice -> "2223222B22232224"
    | `Classic -> "2222222222222222"
