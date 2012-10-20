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

(** Single player computer enemy *)

open Game

(** in frames (1/100 seconds) *)
let build_delay = 2000
let link_delay = 100

let cpuid = 0

type cpu = {
  start: int; (** time the chain started *)
  chain: int; (** link number in the current chain *)
  level: int; (** current level *)
  next_level: int; (** remaining chains in current level *)
}

let start = {
  start = build_delay;
  chain = 0;
  level = 1;
  next_level = 2;
}

let finish game cpu =
  let level, next_level =
    if cpu.next_level <= 1 then
      cpu.level + 1, 3
    else
      cpu.level, cpu.next_level - 1
  in
  let cpu =
    { start = game.now + build_delay;
      chain = 0;
      level = level;
      next_level = next_level }
  in
  cpu

let send_link game cpu =
  let chain = cpu.chain + 1 in
  let cpu = { cpu with chain = chain } in
(*  let garb = ceil_div (40 * (chain_mult game chain + 4)) 120 in*)
  let garb = ceil_div (40 * (chain_power chain + 4)) score_per_garbage in
  if chain >= cpu.level then
    [ Action.SendGarbage (cpuid, garb);
      Action.FinishGarbage cpuid ], finish game cpu
  else
    [ Action.SendGarbage (cpuid, garb) ], cpu

let think game cpu =
  match game.state with
    | GameOver _ -> [], cpu
    | _ ->
        if game.now >= cpu.start then
          if (cpu.start - game.now) mod link_delay = 0 then
            send_link game cpu
          else
            [], cpu
        else
          [], cpu
