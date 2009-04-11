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
  Net.send cx (GetScores 0)

let high_scores_screen cx =
  Draw.draw_empty ();
  try
    Menu.waiting_string "CONNECTED"
      (fun () -> List.iter (function
                              | Score (pos, name, score) ->
                                  Printf.printf "Score %2d: %s, %d\n%!"
                                    pos name (Score.score score)
                              | _ -> ()) (Net.receive cx); None);
  with Exit ->
    ()
