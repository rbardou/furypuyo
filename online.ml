(** Multi-player games *)

open Game
open Misc
open Protocol
open ToServer
open ToClient
open Sprites
open Common

exception GameStarts

let rec check_message cx test () =
  match Net.receive_one cx with
    | None -> None
    | Some m ->
	match test m with
	  | None -> check_message cx test ()
	  | Some _ as x -> x

exception StopMessageIteration

let rec iterate_messages f cx =
  match Net.receive_one cx with
    | None -> ()
    | Some m ->
        try
          f m;
          iterate_messages f cx
        with StopMessageIteration ->
          ()

let option_of_bool = function
  | true -> Some ()
  | false -> None

let connection_screen () =
  Draw.draw_empty ();
  let host =
    Menu.input_string
      ~default: (Config.get server_address)
      "ENTER SERVER ADDRESS:"
  in
  Config.set server_address host;
  let cx = Net.connect host (Config.get server_port) in
  try
    Draw.draw_empty ();
    Menu.waiting_string "CONNECTING" (fun () -> option_of_bool (Net.ready cx));
    Draw.draw_empty ();
    let name =
      Menu.input_string
        ~default: (Config.get player_name)
        "ENTER YOUR NAME:"
    in
    Config.set player_name name;
    Net.send cx (MyName name);
    Draw.draw_empty ();
    let name_exists =
      Menu.waiting_string
        "CONNECTING"
        (check_message cx (function YourNameExists e -> Some e | _ -> None))
    in
    if name_exists then begin
      Draw.draw_empty ();
      let pass =
        Menu.input_string
          ~default: ""
          ~passchar: ':'
          "ENTER YOUR PASSWORD:"
      in
      Net.send cx (MyPassword pass);
      let ok =
        Menu.waiting_string
          "CONNECTING"
          (check_message cx (function
                               | YouAreConnected -> Some true
                               | WrongPassword -> Some false
                               | _ -> None))
      in
      if not ok then begin
        Draw.draw_empty ();
        Menu.waiting_string "BAD PASSWORD" (fun () -> None);
      end
    end else begin
      let pass = ref "" in
      begin
        try
          while true do
            Draw.draw_empty ();
            let pass1 =
              Menu.input_string
                ~default: ""
                ~passchar: ':'
                "ENTER NEW PASSWORD:"
            in
            Draw.draw_empty ();
            let pass2 =
              Menu.input_string
                ~default: ""
                ~passchar: ':'
                "CONFIRM NEW PASSWORD:"
            in
            if pass1 = pass2 then begin
              pass := pass1;
              raise Exit
            end
          done
        with Exit ->
          ()
      end;
      Net.send cx (MyPassword !pass);
      Draw.draw_empty ();
      Menu.waiting_string
        "CONNECTING"
        (check_message cx (function
                             | YouAreConnected -> Some ()
                             | _ -> None))
    end;
    Some (cx, name)
  with Exit ->
    Net.close cx;
    None

let send_score cx scores =
  begin match scores with
    | [] -> ()
    | score :: _ -> Net.send cx (MyScore score)
  end

open MenuAction

let high_scores_screen cx =
  let empty = "", Score.make 0 in
  let count = 10 in
  let scores = Array.make count empty in
  let pos = ref 1 in
  Draw.draw_empty ();
  let background = IO.Sprite.screenshot () in
  MenuReader.reset ();
  let set_score p s =
    if p >= !pos && p < !pos + count then
      scores.(p - !pos) <- s
  in
  Net.send cx (GetScores 0);
  let shift_position i =
    let old = !pos in
    pos := !pos + i;
    if !pos < 1 then pos := 1;
    let olds = Array.to_list scores in
    for i = 0 to count - 1 do
      scores.(i) <- empty
    done;
    list_iteri
      (fun i s -> set_score (i + old) s)
      olds;
    Net.send cx (GetScores (!pos - 1))
  in
  try
    while true do
      if IO.frame_delay 10 then begin
        IO.Sprite.draw background 0 0;
        Menu.draw_high_scores_page
          (Menu.high_scores_top_players_page ~pos: !pos (Array.to_list scores))
      end;

      List.iter
        (function
           | Up | Left ->
               shift_position (-1)
           | Down | Right ->
               shift_position 1
           | PageUp ->
               shift_position (-10)
           | PageDown ->
               shift_position 10
           | Return | Escape ->
               raise Exit)
        (MenuReader.read ());

      List.iter
        (function
           | Score (pos, name, score) ->
               set_score (pos + 1) (name, score)
           | _ -> ())
        (Net.receive cx)
    done
  with Exit ->
    ()

(* return true if disconnect, false if quit *)
let rec menu cx login =
  Net.send cx GetRoomList;
  Draw.draw_empty ();
  let joined_room = ref None in
  let rooms =
    try
      Menu.waiting_string
	"LISTING ROOMS..."
	(check_message cx
           (function
              | RoomList l -> Some l
              | JoinedRoom (_, id) -> joined_room := Some id; None
              | _ -> None))
    with Exit ->
      []
  in
  match !joined_room with
    | None ->
        let room_choices =
          List.map (fun (name, id) -> String.uppercase name, `Join id) rooms in
        Draw.draw_empty ();
        let choice =
          Menu.string_choices
            (room_choices @ [
               "CREATE NEW ROOM", `CreateNewRoom;
               "REFRESH ROOM LIST", `RefreshRoomList;
               "HIGH SCORES", `HighScores;
               "DISCONNECT", `Disconnect;
               "QUIT", `Quit;
             ])
        in
        begin match choice with
          | `Join id ->
	      join_room cx login (Some id)
          | `CreateNewRoom ->
	      join_room cx login None
          | `RefreshRoomList ->
              menu cx login
          | `HighScores ->
	      high_scores_screen cx;
	      menu cx login
          | `Disconnect ->
	      Net.close cx;
	      true
          | `Quit ->
	      Net.close cx;
	      false
        end
    | Some id ->
        join_room cx login (Some id)

and join_room cx login rido =
  begin match rido with
    | None -> Net.send cx NewRoom
    | Some id -> Net.send cx (JoinRoom id)
  end;
  Draw.draw_empty ();
  let room =
    try
      let r =
	Menu.waiting_string
	  "JOINING ROOM..."
	  (check_message cx
	     (function JoinedRoom (name, id) -> Some (name, id) | _ -> None))
      in Some r
    with Exit ->
      None
  in
  match room with
    | None -> menu cx login
    | Some (name, id) -> joined_room cx login name id

and joined_room cx login rname rid =
  let rname = String.uppercase rname in
  Draw.draw_empty ();
  let background = IO.Sprite.screenshot () in
  let title_x = screen_width / 2 in
  let title_y = 20 in
  let player_y = 80 in
  let player_x = 20 in
  let player_h = 30 in
  MenuReader.reset ();
  let players = ref [] in
  try
    while true do
      if IO.frame_delay 10 then begin
	IO.Sprite.draw background 0 0;
	IO.Text.write font ~align: IO.Top title_x title_y rname;
	list_iteri
	  (fun i (name, ready) ->
	     let name = String.uppercase name in
	     let text = if ready then "OK "^name else "   "^name in
	     IO.Text.write
	       font
	       ~align: IO.TopLeft player_x
	       (player_y + i * player_h)
	       text)
	  !players;
	IO.update ()
      end;

      List.iter
	(function
           | Escape -> raise Exit
	   | Return -> Net.send cx Ready
           | _ -> ())
	(MenuReader.read ());

      List.iter
	(function
	   | RoomPlayers l -> players := l
	   | StartGame -> raise GameStarts
	   | _ -> ())
	(Net.receive cx)
    done;
    assert false
  with
    | Exit ->
	Net.send cx LeaveRoom;
	menu cx login
    | GameStarts ->
	multi_player_game cx login

and multi_player_game cx login =
  let game = ref (Game.start ()) in
  let replay = Replay.record !game in
  IO.timer_start ();
  let won = ref false in
  let quit = ref false in
  while not (game_finished !game || !won || !quit) do
    let actions = ref (Reader.read ()) in
    if IO.frame_delay 10 then Draw.draw !game;

    iterate_messages
      (function
         | PrepareGarbage i ->
             actions := (Action.SendGarbage i) :: !actions
         | ReadyGarbage i ->
             actions := (Action.FinishSomeGarbage i) :: !actions
         | YouWin ->
             won := true;
             raise StopMessageIteration
         | _ -> ())
      cx;

    Replay.frame replay !actions;
    game := Game.think_frame !game !actions;

    (* send garbage *)
    let garbage = !game.garbage_sent in
    if garbage > 0 then begin
      game := { !game with garbage_sent = 0 };
      Net.send cx (SendGarbage garbage)
    end;
    if !game.garbage_finished then begin
      game := { !game with garbage_finished = false };
      Net.send cx FinishGarbage
    end;

    (* TODO: pause menu *)
    if List.mem Action.Escape !actions then
      quit := true;
  done;
  if not !won then Net.send cx GameOver;
  save_replay replay (login ^ "_online");
  menu cx login
