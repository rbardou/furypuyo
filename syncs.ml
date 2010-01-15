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

open Misc

type player = {
  name: string;
  game: Sync.t;
}

type t = {
  map: player IntMap.t;
  pids: int list;
}

let start_player rand acc (id, name, dropset) =
  let player = {
    name = name;
    game = Sync.create (Generator.of_dropset dropset) rand;
  } in
  IntMap.add id player acc

let create rand ignore_login players =
  let players = List.filter (fun (_, n, _) -> n <> ignore_login) players in
  { map = List.fold_left (start_player rand) IntMap.empty players;
    pids = List.map (fun (a, _, _) -> a) players }

let step m =
  { m with
      map = IntMap.map (fun p -> { p with game = Sync.step p.game }) m.map }

let inputs m pid t l =
  try
    let p = IntMap.find pid m.map in
    { m with
        map = IntMap.add pid { p with game = Sync.inputs p.game t l } m.map }
  with Not_found ->
    m

let get m pid =
  try
    let p = IntMap.find pid m.map in
    Some (p.name, Sync.game p.game)
  with Not_found ->
    None

let next_player m =
  match m.pids with
    | [] -> m
    | a :: r -> { m with pids = r @ [a] }

let current_player m =
  match m.pids with
    | [] -> None
    | a :: _ -> get m a
