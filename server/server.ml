open Protocol
open ToServer
open ToClient
open Misc

let maximum_connection_count = 20

let config =
  Config.init ~var: "FURYPUYOSRVCONF" "~/.furypuyo";
  Config.load "server.cfg" "Fury Puyo Server Configuration File"

let port =
  Config.get (Config.int config "PORT" "Server port" 4269)

type player = {
  pid: int;
  name: string;
  pass: string;
}

let player_identifier = Bin.identifier "PUYOSRVPLAYER"

exception Unknown_file_format

let encode_player buf p =
  Bin.write buf player_identifier ();
  Bin.write buf Bin.int 1; (* version *)
  Bin.write buf Bin.int p.pid;
  Bin.write buf Bin.string p.name;
  Bin.write buf Bin.string p.pass

let decode_player buf =
  Bin.read buf player_identifier;
  match Bin.read buf Bin.int with
    | 1 ->
        let pid = Bin.read buf Bin.int in
        let name = Bin.read buf Bin.string in
        let pass = Bin.read buf Bin.string in
        {
          pid = pid;
          name = name;
          pass = pass;
        }
    | _ -> raise Unknown_file_format

let codec_player =
  Bin.custom encode_player decode_player

type players = {
  players: (string, player) Hashtbl.t;
  mutable next: int;
}

type client_state =
  | Hello
  | Logging of string
  | Logged of player

type client = {
  id: int;
  cx: (Net.message_to_client, Net.message_to_server) Net.connection;
  mutable state: client_state;
}

let client_id c =
  let name =
    match c.state with
      | Hello -> "?"
      | Logging name -> name ^ "?"
      | Logged player -> player.name ^ ":" ^ string_of_int player.pid
  in
  string_of_int c.id ^ ":" ^ name

let logc c x = Printf.ksprintf (log "[%s] %s" (client_id c)) x

let player_file_suffix = ".player"
let players_dir = Config.filename "players"
let player_file pid =
  "players/" ^ string_of_int pid ^ player_file_suffix

let load_player players file =
  try
    let ch = Config.open_in file in
    let player = Bin.read (Bin.from_channel ch) codec_player in
    close_in ch;
    players.next <- max players.next (player.pid + 1);
    Hashtbl.add players.players player.name player;
    log "loaded player: %s (%d)" player.name player.pid
  with
    | Sys_error s ->
        log "WARNING: could not read player file: %s (%s)" file s
    | Unknown_file_format
    | Bin.Bad_identifier _ ->
        log "WARNING: could not read player file: %s (unknown file format)"
          file

let save_player player =
  let file = player_file player.pid in
  try
    let ch = Config.open_out file in
    Bin.write (Bin.to_channel ch) codec_player player;
    close_out ch
  with Sys_error s ->
    log "Warning: could not save player file: %s (%s)" file s

let load_players () =
  let players = {
    players = Hashtbl.create 17;
    next = 0;
  } in
  Hashtbl.clear players.players;
  players.next <- 0;
  if Sys.file_exists players_dir then begin
    let manifest = Sys.readdir players_dir in
    let manifest = Array.to_list manifest in
    let manifest =
      List.filter
        (fun f -> Filename.check_suffix f player_file_suffix)
        manifest
    in
    let manifest = List.map (Filename.concat players_dir) manifest in
    List.iter (load_player players) manifest;
  end;
  players

let new_player players name pass =
  let player = {
    name = name;
    pass = pass;
    pid = players.next;
  } in
  players.next <- players.next + 1;
  Hashtbl.replace players.players name player;
  save_player player;
  log "new player: %s (%d)" name player.pid;
  player

let find_player players name =
  Hashtbl.find players.players name

let mem_player players name =
  Hashtbl.mem players.players name

let handle_client_message players c m =
  match c.state, m with
    | Hello, MyName name ->
        c.state <- Logging name;
        Net.send c.cx (YourNameExists (mem_player players name))
    | Logging name, MyPassword pass ->
        let accept_login player =
          c.state <- Logged player;
          logc c "logged in";
          Net.send c.cx YouAreConnected
        in
        begin try
          let player = find_player players name in
          if player.pass = pass then
            accept_login player
          else
            Net.send c.cx WrongPassword
        with Not_found ->
          let player = new_player players name pass in
          accept_login player
        end
    | _ ->
        ()

let handle_client players c =
  List.iter (handle_client_message players c) (Net.receive c.cx)

let new_client =
  let next = ref 0 in
  fun cx ->
    let id = !next in
    incr next;
    let c = {
      id = id;
      cx = cx;
      state = Hello;
    } in
    logc c "new client: %s:%d" (Net.remote_address cx) (Net.remote_port cx);
    c

let check_active c =
  if Net.active c.cx then
    true
  else begin
    logc c "disconnected";
    false
  end

let () =
  let players = load_players () in
  let server = Net.listen port in
  let clients = ref [] in
  log "listening to port %d" port;
  while true do
    clients := List.filter check_active !clients;
    let news = Net.accept ~max: maximum_connection_count server in
    let news = List.map new_client news in
    clients := !clients @ news;
    List.iter (handle_client players) !clients;
  done
