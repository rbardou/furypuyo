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

(** Multi-player games *)

open Game
open Misc
open Protocol
open ToServer
open ToClient
open Sprites
open Common

exception GameStarts of Rand.t * (int * string * Generator.dropset) list

let sprite_puyo = IO.Sprite.align sprite_puyo_red IO.Center

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
  let cx = ref None in
  try
    let host =
      Menu.input_string
        ~default: (Config.get server_address)
        ~escape: (fun () -> raise Exit)
        "ENTER SERVER ADDRESS:"
    in
    Config.set server_address host;
    cx := begin
      try
        Some (Net.connect host (Config.get server_port))
      with Udp.Network_error (_, s) ->
        Draw.draw_empty ();
        Menu.show_message (String.uppercase s);
        None
    end;
    let cx = match !cx with
      | Some cx -> cx
      | None -> raise Exit
    in
    Draw.draw_empty ();
    Menu.waiting_string "CONNECTING..."
      (fun () -> option_of_bool (Net.ready cx));
    Draw.draw_empty ();
    let name =
      Menu.input_string
        ~default: (Config.get player_name)
        ~escape: (fun () -> raise Exit)
        "ENTER YOUR NAME:"
    in
    Config.set player_name name;
    Net.send cx (MyName name);
    Draw.draw_empty ();
    let name_exists =
      Menu.waiting_string
        "CONNECTING..."
        (check_message cx (function YourNameExists e -> Some e | _ -> None))
    in
    if name_exists then begin
      Draw.draw_empty ();
      let pass =
        Menu.input_string
          ~default: ""
          ~passchar: '*'
          ~escape: (fun () -> raise Exit)
          "ENTER YOUR PASSWORD:"
      in
      Net.send cx (MyPassword pass);
      let ok =
        Menu.waiting_string
          "CONNECTING..."
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
                ~passchar: '*'
                ~escape: (fun () -> raise Not_found)
                "ENTER NEW PASSWORD:"
            in
            Draw.draw_empty ();
            let pass2 =
              Menu.input_string
                ~default: ""
                ~passchar: '*'
                ~escape: (fun () -> raise Not_found)
                "CONFIRM NEW PASSWORD:"
            in
            if pass1 = pass2 then begin
              pass := pass1;
              raise Exit
            end
          done
        with
          | Exit ->
              ()
          | Not_found ->
              raise Exit
      end;
      Net.send cx (MyPassword !pass);
      Draw.draw_empty ();
      Menu.waiting_string
        "CONNECTING..."
        (check_message cx (function
                             | YouAreConnected -> Some ()
                             | _ -> None))
    end;
    Some (cx, name)
  with
    | Exit ->
        begin match !cx with
          | Some cx -> Net.close cx
          | None -> ()
        end;
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
  let handicap_x = 40 in
  let handicap_y = screen_height - 40 in
  let team_x = 40 in
  let team_y = screen_height - 70 in
  let dropset_x = 40 in
  let dropset_y = screen_height - 100 in
  let dropsets = [| `Nice; `Classic |] in
  MenuReader.reset ();
  let players = ref [] in
  let handicap = ref 0 in
  let team = ref 0 in
  let dropset = ref (Config.get player_dropset) in
  Net.send cx (MyDropset !dropset);
  let cursor_x = 20 in
  let cursor_y = ref (float_of_int handicap_y) in
  let cursor_positions = [| `Dropset; `Team; `Handicap |] in
  let cursor_pos = ref `Handicap in
  let time = ref 0 in
  try
    while true do
      incr time;

      if IO.frame_delay 10 then begin
	IO.Sprite.draw background 0 0;
	IO.Text.write font ~align: IO.Top title_x title_y rname;
	list_iteri
	  (fun i (name, ready, handicap, team) ->
	     let name = String.uppercase name in
	     let text = if ready then "OK "^name else "   "^name in
             let text =
               if handicap > 0 && team > 0 then
                 text ^ " (T" ^ string_of_int team ^ ", H"
                 ^ string_of_int handicap ^ ")"
               else if handicap > 0 then
                 text ^ " (H" ^ string_of_int handicap ^ ")"
               else if team > 0 then
                 text ^ " (T" ^ string_of_int team ^ ")"
               else
                 text
             in
	     IO.Text.write
	       font
	       ~align: IO.TopLeft player_x
	       (player_y + i * player_h)
	       text)
	  !players;
        IO.Text.write font ~align: IO.Left dropset_x dropset_y
          (Printf.sprintf "DROPSET: %s"
             (print_dropset dropset ()));
        IO.Text.write font ~align: IO.Left team_x team_y
          (Printf.sprintf "TEAM: %s"
             (if !team = 0 then "NONE" else string_of_int !team));
        IO.Text.write font ~align: IO.Left handicap_x handicap_y
          (if !handicap = 0 then
             "HANDICAP: NONE"
           else
             Printf.sprintf "HANDICAP: %d (%d%%)" !handicap
               (percent_of_handicap !handicap - 100));
        IO.Sprite.draw sprite_puyo cursor_x (int_of_float !cursor_y);
	IO.update ()
      end;

      let d = match !cursor_pos with
        | `Team -> team_y
        | `Handicap -> handicap_y
        | `Dropset -> dropset_y
      in
      let d = float_of_int d in
      cursor_y := !cursor_y +. (d -. !cursor_y) /. 10.;

      if !time >= 50 then
        List.iter
	  (function
             | Escape -> raise Exit
	     | Return -> Net.send cx Ready
             | Left ->
                 begin match !cursor_pos with
                   | `Handicap ->
                       decr handicap;
                       if !handicap < 0 then handicap := 0;
                       Net.send cx (MyHandicap !handicap)
                   | `Team ->
                       decr team;
                       if !team < 0 then team := 0;
                       Net.send cx (MyTeam !team)
                   | `Dropset ->
                       Menu.prev dropset dropsets ();
                       Config.set player_dropset !dropset;
                       Net.send cx (MyDropset !dropset)
                 end
             | Right ->
                 begin match !cursor_pos with
                   | `Handicap ->
                       incr handicap;
                       if !handicap > 20 then handicap := 20;
                       Net.send cx (MyHandicap !handicap)
                   | `Team ->
                       incr team;
                       if !team > 9 then team := 9;
                       Net.send cx (MyTeam !team)
                   | `Dropset ->
                       Menu.next dropset dropsets ();
                       Config.set player_dropset !dropset;
                       Net.send cx (MyDropset !dropset)
                 end
             | Up ->
                 Menu.prev cursor_pos cursor_positions ()
             | Down ->
                 Menu.next cursor_pos cursor_positions ()
             | _ -> ())
	  (MenuReader.read ())
      else
        ignore (MenuReader.read ());

      List.iter
	(function
	   | RoomPlayers l -> players := l
	   | StartGame (r, pl) -> raise (GameStarts (r, pl))
           | YourHandicap i -> handicap := i
           | YourTeam i -> team := i
	   | _ -> ())
	(Net.receive cx)
    done;
    assert false
  with
    | Exit ->
	Net.send cx LeaveRoom;
	menu cx login
    | GameStarts (r, pl) ->
	multi_player_game cx login !dropset r pl

and multi_player_game cx login dropset rand players =
  let game =
    ref (Game.start_multiplayer ~generator: (Generator.of_dropset dropset) rand)
  in
  let replay = Replay.record !game in
  IO.timer_start ();
  Reader.reset ();
  let game_over = ref false in
  let won = ref false in
  let quit = ref false in
  let game_over_sent = ref false in
  let other_players = ref (Syncs.create rand login players) in
  while not (!game_over || !quit) do
    let actions = ref (Reader.read ()) in
    if IO.frame_delay 10 then
      Draw.draw_multiplayer !game (Syncs.current_player !other_players);

    iterate_messages
      (function
         | PrepareGarbage (pid, g) ->
             actions := (Action.SendGarbage (pid, g)) :: !actions
         | ReadyGarbage pid ->
             actions := (Action.FinishGarbage pid) :: !actions
         | GameOver b ->
             game_over := true;
             won := b;
             raise StopMessageIteration
         | PlayerInputs (pid, t, l) ->
             other_players := Syncs.inputs !other_players pid t l
         | _ -> ())
      cx;

    Replay.frame replay !actions;
    Net.send cx (MyInputs (!game.now, !actions));
    game := Game.think_frame !game !actions;
    other_players := Syncs.step !other_players;

    (* send garbage *)
    let garbage = !game.garbage_sent in
    if !game.garbage_finished then begin
      game := { !game with garbage_finished = false };
      Net.send cx FinishGarbage
    end;
    if garbage > 0 then begin
      game := { !game with garbage_sent = 0 };
      Net.send cx (SendGarbage garbage)
    end;

    (* TODO: pause menu *)
    if List.mem Action.Escape !actions then
      quit := true;

    if List.mem Action.ViewOtherPlayer !actions then
      other_players := Syncs.next_player !other_players;

    if not !game_over_sent && (game_finished !game || !quit) then begin
      Net.send cx (ILose !quit);
      game_over_sent := true
    end;
  done;
  save_replay replay (login ^ "_online");
  menu cx login
