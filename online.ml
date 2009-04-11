open Misc
open Protocol
open ToServer
open ToClient
open Common

let check_message cx test () =
  let seen = ref None in
  List.iter
    (fun m ->
       match test m with
         | None -> ()
         | Some x -> seen := Some x)
    (Net.receive cx);
  !seen

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
  end;

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
