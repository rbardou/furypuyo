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
  let ch = open_out file in
  let buf = Bin.to_channel ch in
  Bin.write buf Replay.codec replay;
  close_out ch

let rec percent_of_handicap = function
  | 0 -> 100
  | i ->
      let p = percent_of_handicap (i - 1) in
      p + p / 5
