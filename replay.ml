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
  r.current <- r.start

let next r =
  let previous = r.current in
  let actions =
    match r.remaining with
      | [] -> []
      | a :: rem ->
          r.remaining <- rem;
          a
  in
  let game = Game.think_frame r.current actions in
  r.current <- game;
  previous

let codec_cell =
  Bin.convert
    (fun c -> c.Cell.puyo)
    (fun p -> { Cell.puyo = p })
    (Bin.option Puyo.codec)

let encode_game_state buf s =
  let w x = Bin.write buf x in
  match s with
    | Starting s ->
        w Bin.int 0;
        w Bin.int s.s_countdown;
        w Bin.int s.s_next
    | Incoming s ->
        w Bin.int 1;
        w Block.codec s.inc_block;
        w Bin.int s.inc_x;
        w Bin.int s.inc_y;
        w Bin.int s.inc_insert_time;
        w Bin.bool s.inc_fast_fall
    | Inserting s ->
        w Bin.int 2;
        w Bin.int s.ins_end
    | Falling s ->
        w Bin.int 3;
        w (Bin.list (Bin.triple Bin.int Bin.int Puyo.codec)) s.f_puyos;
        w Bin.int s.f_y;
        w Bin.int s.f_speed
    | Popping s ->
        w Bin.int 4;
        w Bin.int s.pop_end;
        w (Bin.list (Bin.couple Bin.int Bin.int)) s.pop_puyos;
        w Bin.int s.pop_score_base;
        w Bin.int s.pop_score_mult;
        w Bin.int s.pop_chain
    | GameOver s ->
        w Bin.int 5;
        w Bin.int s.go_speed;
        w Bin.int s.go_y;
        w Bin.int s.go_end

let decode_game_state buf =
  let r x = Bin.read buf x in
  match r Bin.int with
    | 0 ->
        let countdown = r Bin.int in
        let next = r Bin.int in
        Starting {
          s_countdown = countdown;
          s_next = next;
        }
    | 1 ->
        let block = r Block.codec in
        let x = r Bin.int in
        let y = r Bin.int in
        let insert_time = r Bin.int in
        let fast_fall = r Bin.bool in
        Incoming {
          inc_block = block;
          inc_x = x;
          inc_y = y;
          inc_insert_time = insert_time;
          inc_fast_fall = fast_fall;
        }
    | 2 ->
        let ins_end = r Bin.int in
        Inserting {
          ins_end = ins_end;
        }
    | 3 ->
        let puyos = r (Bin.list (Bin.triple Bin.int Bin.int Puyo.codec)) in
        let y = r Bin.int in
        let speed = r Bin.int in
        Falling {
          f_puyos = puyos;
          f_y = y;
          f_speed = speed;
        }
    | 4 ->
        let pop_end = r Bin.int in
        let puyos = r (Bin.list (Bin.couple Bin.int Bin.int)) in
        let score_base = r Bin.int in
        let score_mult = r Bin.int in
        let chain = r Bin.int in
        Popping {
          pop_end = pop_end;
          pop_puyos = puyos;
          pop_score_base = score_base;
          pop_score_mult = score_mult;
          pop_chain = chain;
        }
    | 5 ->
        let speed = r Bin.int in
        let y = r Bin.int in
        let go_end = r Bin.int in
        GameOver {
          go_speed = speed;
          go_y = y;
          go_end = go_end;
        }
    | _ -> failwith "Replay.decode_game_state"

let codec_game_state =
  Bin.custom encode_game_state decode_game_state

let encode_game_speed buf s =
  let w x = Bin.write buf x in
  w Bin.int s.sp_fall_absorb;
  w Bin.int s.sp_fall;
  w Bin.int s.sp_fall_fast;
  w Bin.int s.sp_insert_delay;
  w Bin.int s.sp_gravity;
  w Bin.int s.sp_pop_delay;
  w Bin.int s.sp_fury_initial_delay;
  w Bin.int s.sp_fury_acceleration;
  w Bin.int s.sp_fury_minimum_delay;
  w Bin.int s.sp_fury_initial;
  w Bin.int s.sp_fury_gravity;
  w Bin.int s.sp_fury_pop_delay

let decode_game_speed buf =
  let r x = Bin.read buf x in
  let sp_fall_absorb = r Bin.int in
  let sp_fall = r Bin.int in
  let sp_fall_fast = r Bin.int in
  let sp_insert_delay = r Bin.int in
  let sp_gravity = r Bin.int in
  let sp_pop_delay = r Bin.int in
  let sp_fury_initial_delay = r Bin.int in
  let sp_fury_acceleration = r Bin.int in
  let sp_fury_minimum_delay = r Bin.int in
  let sp_fury_initial = r Bin.int in
  let sp_fury_gravity = r Bin.int in
  let sp_fury_pop_delay = r Bin.int in
  {
    sp_fall_absorb = sp_fall_absorb;
    sp_fall = sp_fall;
    sp_fall_fast = sp_fall_fast;
    sp_insert_delay = sp_insert_delay;
    sp_gravity = sp_gravity;
    sp_pop_delay = sp_pop_delay;
    sp_fury_initial_delay = sp_fury_initial_delay;
    sp_fury_acceleration = sp_fury_acceleration;
    sp_fury_minimum_delay = sp_fury_minimum_delay;
    sp_fury_initial = sp_fury_initial;
    sp_fury_gravity = sp_fury_gravity;
    sp_fury_pop_delay = sp_fury_pop_delay;
  }

let codec_game_speed =
  Bin.custom encode_game_speed decode_game_speed

let encode_fury_state buf f =
  let w x = Bin.write buf x in
  match f with
    | FNone ->
        w Bin.int 0
    | FInitial i ->
        w Bin.int 1;
        w Bin.int i
    | FDown (a, b) ->
        w Bin.int 2;
        w Bin.int a;
        w Bin.int b

let decode_fury_state buf =
  let r x = Bin.read buf x in
  match r Bin.int with
    | 0 ->
        FNone
    | 1 ->
        let i = r Bin.int in
        FInitial i
    | 2 ->
        let a = r Bin.int in
        let b = r Bin.int in
        FDown (a, b)
    | _ -> failwith "Replay.decode_fury_state"

let codec_fury_state =
  Bin.custom encode_fury_state decode_fury_state

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
  w codec_fury_state game.fury
  (* do not save Gfx *)

let decode_game buf =
  let r x = Bin.read buf x in
  let now = r Bin.int in
  let field = r (Matrix.codec codec_cell) in
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
    garbage_sent = 0; (* not useful for replays anyway *)
    garbage_finished = false; (* not useful for replays anyway *)
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
