open Protocol
open ToServer
open ToClient
open Misc

let maximum_connection_count = 20
let maximum_score_count = 10
let maximum_room_count = 5

let config =
  Config.init ~var: "FURYPUYOSRVCONF" "~/.furypuyo";
  Config.load "server.cfg" "Fury Puyo Server Configuration File"

let port =
  Config.get (Config.int config "PORT" "Server port" 4269)

type player = {
  pid: int;
  name: string;
  pass: string;
  mutable best_score: Score.t;
  mutable room: room option;
  mutable pcx: (Net.message_to_client, Net.message_to_server) Net.connection option;
  mutable ready: bool;
}

and room = {
  rid: int;
  rname: string;
  mutable rplayers: player list;
}

let player_identifier = Bin.identifier "PUYOSRVPLAYER"

exception Unknown_file_format

let send_to p m =
  match p.pcx with
    | None -> log "warning: cannot send to player %s (%d)" p.name p.pid
    | Some cx -> Net.send cx m

let encode_player buf p =
  Bin.write buf player_identifier ();
  Bin.write buf Bin.int 1; (* version *)
  Bin.write buf Bin.int p.pid;
  Bin.write buf Bin.string p.name;
  Bin.write buf Bin.string p.pass;
  Bin.write buf Score.codec p.best_score

let decode_player buf =
  Bin.read buf player_identifier;
  match Bin.read buf Bin.int with
    | 1 ->
        let pid = Bin.read buf Bin.int in
        let name = Bin.read buf Bin.string in
        let pass = Bin.read buf Bin.string in
        let best_score = Bin.read buf Score.codec in
        {
          pid = pid;
          name = name;
          pass = pass;
          best_score = best_score;
	  room = None;
	  pcx = None;
	  ready = false;
        }
    | _ -> raise Unknown_file_format

let codec_player =
  Bin.custom encode_player decode_player

module NamedScore = struct
  type t = int * Score.t
  let compare (n1, s1) (n2, s2) =
    match compare s2 s1 with
      | 0 -> compare n1 n2
      | n -> n
end
module ScoreList = SortedList(NamedScore)

type server_state = {
  players: (string, player) Hashtbl.t;
  players_by_id: (int, player) Hashtbl.t;
  mutable next: int;
  mutable scores: ScoreList.t;
  mutable rooms: room list;
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
    Hashtbl.add players.players_by_id player.pid player;
    players.scores <-
      ScoreList.add (player.pid, player.best_score) players.scores;
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
    players_by_id = Hashtbl.create 17;
    next = 0;
    scores = ScoreList.empty;
    rooms = [];
  } in
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
    best_score = Score.make 0;
    room = None;
    pcx = None;
    ready = false;
  } in
  players.next <- players.next + 1;
  Hashtbl.add players.players name player;
  Hashtbl.add players.players_by_id player.pid player;
  players.scores <-
    ScoreList.add (player.pid, player.best_score) players.scores;
  save_player player;
  log "new player: %s (%d)" name player.pid;
  player

let find_player players name =
  Hashtbl.find players.players name

let find_player_by_id players name =
  Hashtbl.find players.players_by_id name

let mem_player players name =
  Hashtbl.mem players.players name

let fresh_room_id rooms =
  let l = List.map (fun r -> r.rid) rooms in
  let l = List.sort compare l in
  let rec aux n = function
    | [] -> n
    | i :: r when i = n -> aux (n + 1) r
    | _ -> n
  in
  aux 0 l

let create_new_room players name =
  let room = {
    rid = fresh_room_id players.rooms;
    rname = name;
    rplayers = [];
  } in
  log "room created: %s (%d)" room.rname room.rid;
  room

let destroy_room players room =
  log "room destroyed: %s (%d)" room.rname room.rid;
  players.rooms <- List.filter (fun r -> r.rid <> room.rid) players.rooms

let room_send_players room =
  let m = RoomPlayers (List.map (fun p -> p.name, p.ready) room.rplayers) in
  List.iter
    (fun p -> send_to p m)
    room.rplayers

let player_join_room c player room =
  player.room <- Some room;
  player.ready <- false;
  logc c "joined room %s (%d)" room.rname room.rid;
  room.rplayers <- player :: room.rplayers;
  Net.send c.cx (JoinedRoom (room.rname, room.rid));
  room_send_players room

let player_ready c player =
  match player.room with
    | None -> ()
    | Some room ->
	player.ready <- true;
	room_send_players room

let player_leave_room players c player =
  match player.room with
    | None -> ()
    | Some room ->
	logc c "leaved room %s (%d)" room.rname room.rid;
	player.room <- None;
	room.rplayers <- List.filter (fun p -> p.pid <> player.pid) room.rplayers;
	room_send_players room;
	match room.rplayers with
	  | [] -> destroy_room players room
	  | _ -> ()

let handle_client_message players c m =
  match c.state, m with
    | Hello, MyName name ->
        c.state <- Logging name;
        Net.send c.cx (YourNameExists (mem_player players name))
    | Logging name, MyPassword pass ->
        let accept_login player =
          c.state <- Logged player;
	  player.pcx <- Some c.cx;
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
    | Logged player, MyScore score ->
        players.scores <-
          ScoreList.remove (player.pid, player.best_score) players.scores;
        player.best_score <- Score.max player.best_score score;
        players.scores <-
          ScoreList.add (player.pid, player.best_score) players.scores;
        save_player player
    | Logged _, GetScores pos ->
        list_iteri
          (fun i (pid, score) ->
             Net.send c.cx
               (Score (pos + i, (find_player_by_id players pid).name, score)))
          (ScoreList.sub pos (pos + 9) players.scores)
    | Logged _, GetRoomList ->
	let rooms = List.map (fun r -> r.rname, r.rid) players.rooms in
	Net.send c.cx (RoomList rooms)
    | Logged player, NewRoom ->
	begin match player.room with
	  | None ->
	      if List.length players.rooms < maximum_room_count then begin
		let room = create_new_room players (player.name ^ "'s room") in
		players.rooms <- room :: players.rooms;
		player_join_room c player room;
	      end else
		() (* TODO: tell client the error *)
	  | Some room ->
	      Net.send c.cx (JoinedRoom (room.rname, room.rid))
	end
    | Logged player, JoinRoom rid ->
	begin match player.room with
	  | None ->
	      begin try
		let room = List.find (fun r -> r.rid = rid) players.rooms in
		player_join_room c player room
	      with Not_found ->
		() (* TODO: tell client the error *)
	      end
	  | Some room ->
	      Net.send c.cx (JoinedRoom (room.rname, room.rid))
	end
    | Logged player, LeaveRoom ->
	player_leave_room players c player
    | Logged player, Ready ->
	player_ready c player
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

let deactivate_client players c =
  logc c "disconnected";
  match c.state with
    | Hello | Logging _ ->
	()
    | Logged player ->
	player.pcx <- None;
	player_leave_room players c player

let () =
  let players = load_players () in
  let server = Net.listen port in
  let clients = ref [] in
  log "listening to port %d" port;
  while true do
    let active, inactive = List.partition (fun c -> Net.active c.cx) !clients in
    clients := active;
    List.iter (deactivate_client players) inactive;
    let news = Net.accept ~max: maximum_connection_count server in
    let news = List.map new_client news in
    clients := !clients @ news;
    List.iter (handle_client players) !clients;
  done
