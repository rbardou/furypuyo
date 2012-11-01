open Game
open Common

module MultiAction =
struct
  type t = int * Action.t
end

module MultiReader = IO.MakeReader(MultiAction)

let () =
  (* Player 1 keys *)
  MultiReader.key_down Sdlkey.KEY_ESCAPE (1, Action.Escape);
  MultiReader.key_down Sdlkey.KEY_LEFT (1, Action.MLeft);
  MultiReader.key_down Sdlkey.KEY_RIGHT (1, Action.MRight);
  MultiReader.key_up Sdlkey.KEY_LEFT (1, Action.MLeftRelease);
  MultiReader.key_up Sdlkey.KEY_RIGHT (1, Action.MRightRelease);
  MultiReader.key_down Sdlkey.KEY_UP (1, Action.RRight);
  MultiReader.key_down Sdlkey.KEY_RCTRL (1, Action.RLeft);
  MultiReader.key_down Sdlkey.KEY_LCTRL (1, Action.RLeft);
  MultiReader.key_down Sdlkey.KEY_LALT (1, Action.RRight);
  MultiReader.key_down Sdlkey.KEY_KP1 (1, Action.RRight);
  MultiReader.key_down Sdlkey.KEY_SPACE (1, Action.InstaFall);
  MultiReader.key_down Sdlkey.KEY_d (1, Action.Debug);
  MultiReader.key_down Sdlkey.KEY_DOWN (1, Action.MDown);
  MultiReader.key_up Sdlkey.KEY_DOWN (1, Action.MDownRelease);
  MultiReader.key_down Sdlkey.KEY_TAB (1, Action.ViewOtherPlayer);

  (* Player 2 keys *)
  MultiReader.key_down Sdlkey.KEY_k (2, Action.MLeft);
  MultiReader.key_down Sdlkey.KEY_m (2, Action.MRight);
  MultiReader.key_up Sdlkey.KEY_k (2, Action.MLeftRelease);
  MultiReader.key_up Sdlkey.KEY_m (2, Action.MRightRelease);
  MultiReader.key_down Sdlkey.KEY_q (2, Action.RLeft);
  MultiReader.key_down Sdlkey.KEY_s (2, Action.RRight);
  MultiReader.key_down Sdlkey.KEY_o (2, Action.InstaFall);
  MultiReader.key_down Sdlkey.KEY_l (2, Action.MDown);
  MultiReader.key_up Sdlkey.KEY_l (2, Action.MDownRelease)

let filter_player_actions n actions =
  let l = List.filter (fun (i, _) -> i = n) actions in
  List.map snd l

let two_player_game () =
  let generator = Generator.of_dropset `Classic in
  let rand = Rand.self_init () in
  let game1 =
    ref (Game.start_multiplayer ~generator rand)
  in
  let game2 =
    ref (Game.start_multiplayer ~generator rand)
  in
  IO.timer_start ();
  MultiReader.reset ();
  let game_over = ref false in
  let quit = ref false in

  let replay1 = Replay.record !game1 in
  let replay2 = Replay.record !game2 in

  while not (!game_over || !quit) do
    let actions = MultiReader.read () in
    let actions1 = ref (filter_player_actions 1 actions) in
    let actions2 = ref (filter_player_actions 2 actions) in

    if IO.frame_delay 10 then
      Draw.draw_multiplayer !game1 (Some ("", !game2));

    (* send garbage *)
    if !game1.garbage_finished then
      begin
        game1 := { !game1 with garbage_finished = false };
        actions2 := (Action.FinishGarbage 1) :: !actions2
      end;
    let garbage_from1 = !game1.garbage_sent in
    if garbage_from1 > 0 then
      begin
        game1 := { !game1 with garbage_sent = 0 };
        actions2 := (Action.SendGarbage (1, garbage_from1)) :: !actions2
      end;

    (* send garbage *)
    if !game2.garbage_finished then
      begin
        game2 := { !game2 with garbage_finished = false };
        actions1 := (Action.FinishGarbage 2) :: !actions1
      end;
    let garbage_from2 = !game2.garbage_sent in
    if garbage_from2 > 0 then
      begin
        game2 := { !game2 with garbage_sent = 0 };
        actions1 := (Action.SendGarbage (2, garbage_from2)) :: !actions1
      end;

    Replay.frame replay1 !actions1;
    Replay.frame replay2 !actions2;

    game1 := Game.think_frame !game1 !actions1;
    game2 := Game.think_frame !game2 !actions2;

    (* TODO: pause menu *)
    if List.mem Action.Escape !actions1 then
      quit := true;
  done;

  save_two_player_replay replay1 replay2 "two_player"
