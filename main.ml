module Reader = IO.MakeReader(Action)

let rec loop game cpu =
  let game = List.fold_left Game.act game (Reader.read ()) in
  let game = Game.think game in
  let game, cpu = Cpu.think game cpu in
  Draw.draw game;
  IO.frame_delay 10;
  loop game cpu

let () =
  let game = Game.start () in
  let cpu = Cpu.start in
  Reader.key_down Sdlkey.KEY_ESCAPE Action.Quit;
  Reader.key_auto 100 30 Sdlkey.KEY_LEFT Action.MLeft;
  Reader.key_auto 100 30 Sdlkey.KEY_RIGHT Action.MRight;
  Reader.key_down Sdlkey.KEY_UP Action.RRight;
  Reader.key_down Sdlkey.KEY_RCTRL Action.RLeft;
  Reader.key_down Sdlkey.KEY_LCTRL Action.RLeft;
  Reader.key_down Sdlkey.KEY_LALT Action.RRight;
  Reader.key_down Sdlkey.KEY_KP0 Action.RRight;
  Reader.key_down Sdlkey.KEY_SPACE Action.InstaFall;
  Reader.key_down Sdlkey.KEY_d Action.Debug;
  Reader.key_continuous Sdlkey.KEY_DOWN Action.MDown;
  loop game cpu
