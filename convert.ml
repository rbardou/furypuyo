(* Convert old (0.3dev) high scores *)

type old = {
  size: int;
  file: string;
  players: int list StringMap.t;
}

module HighScores = Highscores.Make(Score)

let () =
  let x = Marshal.from_channel stdin in
  
