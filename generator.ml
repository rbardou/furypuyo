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

open Block
open Puyo

type state = int

type t = {
  generator: t -> Rand.t -> Rand.t * t * Block.t;
  state: int;
}

let combinations l =
  List.flatten (List.map (fun x -> List.map (fun y -> x, y) l) l)

let next_colors f origin =
  let rec next acc color =
    let nc = f color in
    if nc = origin then acc else next (nc :: acc) nc
  in
  List.rev (next [] origin)

let next_in_list l c =
  match l with
    | [] -> c
    | hd::tl ->
        let rec find = function
          | [] | [_] -> hd
          | x::y::r -> if x = c then y else find (y::r)
        in
        find l

let twos colors =
  List.map
    (fun (a, b) -> List0 [ 0, 0, Puyo.make a;
                           0, 1, Puyo.make b ])
    (combinations colors)

let threes1 colors =
  List.map
    (fun (a, b) -> List1 [ 0, 0, Puyo.make a;
                           0, 1, Puyo.make a;
                           1, 0, Puyo.make b ])
    (combinations colors)

let threes2 colors =
  List.map
    (fun (a, b) -> List0 [ 0, 0, Puyo.make a;
                           0, 1, Puyo.make a;
                           1, 1, Puyo.make b ])
    (combinations colors)

let twotwos colors =
  let combs = combinations colors in
  let combs = List.filter (fun (a, b) -> a <> b) combs in
  List.map
    (fun (a, b) -> List2 [ 0, 0, Puyo.make a;
                           0, 1, Puyo.make a;
                           1, 0, Puyo.make b;
                           1, 1, Puyo.make b ])
    combs

let quads colors =
  List.map (fun a -> Quad (a, next_colors (next_in_list colors) a)) colors

let all_blocks colors =
  twos colors @ threes1 colors @ threes2 colors @ twotwos colors @ quads colors

let random_from blocks =
  let blocks = Array.of_list blocks in
  let count = Array.length blocks in
  let gen state rand =
    let rand, n = Rand.int rand count in
    rand, state, blocks.(n)
  in {
    generator = gen;
    state = 0;
  }

let random colors =
  random_from (all_blocks colors)

let only_twos colors =
  random_from (twos colors)

let sequence l =
  let blocks = Array.of_list (List.map Array.of_list l) in
  let gen state rand =
    let pos = state.state in
    let pos = if pos < 0 || pos >= Array.length blocks then 0 else pos in
    let blocks = blocks.(pos) in
    let rand, n = Rand.int rand (Array.length blocks) in
    rand, { state with state = pos + 1 }, blocks.(n)
  in {
    generator = gen;
    state = 0;
  }

let nice colors =
  sequence [
    twos colors;
    twos colors;
    twos colors;
    quads colors @ twotwos colors;
    twos colors;
    twos colors;
    twos colors;
    threes1 colors @ threes2 colors;
  ]

let next gen rand = gen.generator gen rand
