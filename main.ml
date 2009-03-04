module Reader = IO.MakeReader(Action)

let rec loop game =
  let game = List.fold_left Game.act game (Reader.read ()) in
  let game = Game.think game in
  Draw.draw game;
  IO.frame_delay 10;
  loop game

let () =
  let game = Game.start () in
  Reader.key_down Sdlkey.KEY_ESCAPE Action.Quit;
  Reader.key_down Sdlkey.KEY_LEFT Action.MLeft;
  Reader.key_down Sdlkey.KEY_RIGHT Action.MRight;
  Reader.key_down Sdlkey.KEY_UP Action.RLeft;
  Reader.key_down Sdlkey.KEY_RCTRL Action.RLeft;
  Reader.key_down Sdlkey.KEY_LCTRL Action.RLeft;
  Reader.key_down Sdlkey.KEY_LALT Action.RRight;
  Reader.key_down Sdlkey.KEY_KP0 Action.RRight;
  Reader.key_down Sdlkey.KEY_DOWN Action.MDown;
  loop game
