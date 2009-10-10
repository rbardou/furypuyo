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
