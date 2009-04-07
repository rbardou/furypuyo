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
  name: string;
  pass: string;
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

let load_players players =
  Hashtbl.clear players

let new_player players name pass =
  let player = {
    name = name;
    pass = pass;
  } in
  Hashtbl.replace players name player;
  player

let handle_client_message players c m =
  match c.state, m with
    | Hello, MyName name ->
        c.state <- Logging name;
        Net.send c.cx (YourNameExists (Hashtbl.mem players name))
    | Logging name, MyPassword pass ->
        begin try
          let player = Hashtbl.find players name in
          if player.pass = pass then begin
            c.state <- Logged player;
            Net.send c.cx YouAreConnected
          end else
            Net.send c.cx WrongPassword
        with Not_found ->
          let player = new_player players name pass in
          c.state <- Logged player;
          Net.send c.cx YouAreConnected
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
    log "[%d] new client: %s:%d" id
      (Net.remote_address cx) (Net.remote_port cx);
    {
      id = id;
      cx = cx;
      state = Hello;
    }

let () =
  let players = Hashtbl.create 7 in
  load_players players;
  let server = Net.listen port in
  let clients = ref [] in
  log "Listening to port %d" port;
  while true do
    clients := List.filter (fun c -> Net.active c.cx) !clients;
    let news = Net.accept ~max: maximum_connection_count server in
    let news = List.map new_client news in
    clients := !clients @ news;
    List.iter (handle_client players) !clients;
  done
