(* Convert old (0.3dev) high scores *)

module StringMap = Map.Make(String)

type old = {
  size: int;
  file: string;
  players: int list StringMap.t;
}

module HighScores = Highscores.Make(Score)

let () =
  let x = Marshal.from_channel stdin in
  let h =
    StringMap.fold
      (fun player scores acc ->
         List.fold_left
           (fun acc score ->
              Printf.printf "%s: %d\n%!" player score;
              fst (HighScores.add acc player (Score.make score)))
           acc scores)
      x.players
      (HighScores.empty x.size)
  in
  HighScores.save h "./converted.scores";
  print_endline "Saved to \"./converted.scores\""
