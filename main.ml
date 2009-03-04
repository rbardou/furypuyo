module Reader = IO.MakeReader(Action)

let rec loop game =
  let game = List.fold_left Game.act game (Reader.read ()) in
  Draw.draw game;
  IO.frame_delay 10;
  loop game

let () =
  let game = Game.start () in
  Reader.key_down Sdlkey.KEY_ESCAPE Action.Quit;
  Reader.key_down Sdlkey.KEY_LEFT Action.Left;
  Reader.key_down Sdlkey.KEY_RIGHT Action.Right;
  Reader.key_down Sdlkey.KEY_UP Action.Up;
  Reader.key_down Sdlkey.KEY_DOWN Action.Down;
  loop game
