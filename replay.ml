open Game
open Puyo

let identifier = Bin.identifier "PUYOREPLAY"

type t = {
  start: Game.game;
  mutable actions: Action.t list list; (* in reverse order *)
  mutable current: Game.game;
  mutable remaining: Action.t list list;
}

let record game = {
  start = game;
  actions = [];
  current = game;
  remaining = [];
}

let frame r a =
  r.actions <- a :: r.actions

let play r =
  r.remaining <- List.rev r.actions;
  r.current <- r.start;
  r.start

let next r =
  let actions =
    match r.remaining with
      | [] -> []
      | a :: rem ->
          r.remaining <- rem;
          a
  in
  let game = Game.think_frame r.current actions in
  r.current <- game;
  game

let codec_cell =
  Bin.convert
    (fun c -> c.Cell.puyo)
    (fun p -> { Cell.puyo = p })
    (Bin.option codec_puyo)

let encode_game buf game =
  let w x = Bin.write buf x in
  w Bin.int game.now;
  w (Matrix.codec codec_cell) game.field;
  w Rand.codec game.rand;
  w Generator.codec game.generator;
  w codec_game_state game.state;
  w Bin.int game.score;
  w codec_game_speed game.speed;
  w Bin.int game.chain;
  w (Bin.list Block.codec) game.next_blocks;
  w Bin.int game.garbage_incoming;
  w Bin.int game.garbage_ready;
  w Bin.bool game.garbage_protection;
  w Bin.int game.garbage_position;
  w Bin.int game.offsets;
  w codec_fury_state game.fury_state
  (* do not save Gfx *)

let decode_game buf =
  let r = Bin.read buf in
  let now = r Bin.int game.now in
  let field = r (Matrix.codec Cell.codec) in
  let rand = r Rand.codec in
  let generator = r Generator.codec in
  let state = r codec_game_state in
  let score = r Bin.int in
  let speed = r codec_game_speed in
  let chain = r Bin.int in
  let next_blocks = r (Bin.list Block.codec) in
  let garbage_incoming = r Bin.int in
  let garbage_ready = r Bin.int in
  let garbage_protection = r Bin.bool in
  let garbage_position = r Bin.int in
  let offsets = r Bin.int in
  let fury = r codec_fury_state in
  {
    now = now;
    field = field;
    rand = rand;
    generator = generator;
    state = state;
    score = score;
    speed = speed;
    chain = chain;
    next_blocks = next_blocks;
    garbage_incoming = garbage_incoming;
    garbage_ready = garbage_ready;
    garbage_protection = garbage_protection;
    garbage_position = garbage_position;
    offsets = offsets;
    fury = fury;
    gfx = Gfx.empty;
  }

let codec_game =
  Bin.custom encode_game decode_game

let codec_actions =
  Bin.list (Bin.list Action.codec)

let encode buf replay =
  Bin.write buf identifier ();
  Bin.write buf codec_game replay.start;
  Bin.write buf codec_actions replay.actions

let decode buf =
  Bin.read buf identifier;
  let game = Bin.read buf codec_game in
  let actions = Bin.read buf codec_actions in
  {
    start = game;
    actions = actions;
    current = game;
    remaining = [];
  }

let codec =
  Bin.custom encode decode
