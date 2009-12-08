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

type kind =
  | Two
  | Three
  | Four
  | Big

type t = {
  sequence: kind array;
  position: int;
  color_probabilities: int list;
}

let make_two a b =
  List0 [ 0, 0, Puyo.make a;
          0, 1, Puyo.make b ]

let make_three1 a b =
  List1 [ 0, 0, Puyo.make a;
          0, 1, Puyo.make a;
          1, 0, Puyo.make b ]

let make_three2 a b =
  List0 [ 0, 0, Puyo.make a;
          0, 1, Puyo.make a;
          1, 1, Puyo.make b ]

let make_four a b =
  List2 [ 0, 0, Puyo.make a;
          0, 1, Puyo.make a;
          1, 0, Puyo.make b;
          1, 1, Puyo.make b ]

let make_big a =
  let next =
    match a with
      | Red -> [ Green; Blue; Yellow ]
      | Green -> [ Blue; Yellow; Red ]
      | Blue -> [ Yellow; Red; Green ]
      | Yellow -> [ Red; Green; Blue ]
      | Gray -> assert false
  in
  Quad (a, next)

let random_in list rand =
  let rand, i = Rand.int rand (List.length list) in
  rand, List.nth list i

let colors = [ Red; Green; Blue; Yellow ]

let random_in_probs list rand probs =
  let rec select list probs i =
    match list, probs with
      | c :: rc, p :: rp -> if p > i then c else select rc rp (i - p)
      | _ -> assert false (* impossible *)
  in
  let rand, i = Rand.int rand (List.fold_left (+) 0 probs) in
  rand, select list probs i

let adjust_probs color colors probs =
  let n = List.length colors - 1 in
  List.map2
    (fun c p -> if c = color then p - n else p + 1)
    colors probs

let min_max_probs probs =
  List.map (fun p -> max 0 (min 5 p)) probs

let random_color rand probs =
  let rand, color = random_in_probs colors rand probs in
  let probs = adjust_probs color colors probs in
  let probs = min_max_probs probs in
  rand, probs, color

let random_color_couple_with diff rand probs =
  let rand, color1 = random_in_probs colors rand probs in
  let rec filter f a b =
    match a, b with
      | x :: ra, y :: rb ->
          let rem = filter f ra rb in
          if f x then (x, y) :: rem else rem
      | _ -> assert false (* impossible *)
  in
  let colors2, probs2 =
    if diff then
      List.split (filter ((<>) color1) colors probs)
    else
      colors, probs
  in
  let rand, color2 = random_in_probs colors2 rand probs2 in
  let probs = adjust_probs color1 colors probs in
  let probs = adjust_probs color2 colors probs in
  let probs = min_max_probs probs in
  rand, probs, color1, color2

let random_color_couple = random_color_couple_with false

let random_different_color_couple = random_color_couple_with true

let random_block rand probs = function
  | Two ->
      let rand, probs, c1, c2 = random_color_couple rand probs in
      rand, probs, make_two c1 c2
  | Three ->
      let rand, probs, c1, c2 = random_color_couple rand probs in
      let rand, b = Rand.bool rand in
      rand, probs, (if b then make_three1 else make_three2) c1 c2
  | Four ->
      let rand, probs, c1, c2 = random_different_color_couple rand probs in
      rand, probs, make_four c1 c2
  | Big ->
      let rand, probs, c1 = random_color rand probs in
      rand, probs, make_big c1

let next gen rand =
  let pos = gen.position in
  let new_pos = pos + 1 in
  let new_pos = if new_pos >= Array.length gen.sequence then 0 else new_pos in
  let gen = { gen with position = new_pos } in
  let rand, probs, block =
    random_block rand gen.color_probabilities gen.sequence.(pos) in
  rand, { gen with color_probabilities = probs }, block

let make sequence =
  {
    sequence = sequence;
    position = 0;
    color_probabilities = [ 5; 5; 5; 5 ];
  }

let classic = make [| Two |]

let nice = make [|
  Two; Two; Two; Three;
  Two; Two; Two; Big;
  Two; Two; Two; Three;
  Two; Two; Two; Four
|]

let encode_kind = function
  | Two -> 0
  | Three -> 1
  | Four -> 2
  | Big -> 3

let decode_kind = function
  | 0 -> Two
  | 1 -> Three
  | 2 -> Four
  | 3 -> Big
  | _ -> failwith "Generator.decode_kind"

let codec_kind =
  Bin.convert encode_kind decode_kind Bin.int

let codec =
  Bin.convert
    (fun x -> x.sequence, x.position, x.color_probabilities)
    (fun (s, p, c) -> { sequence = s; position = p; color_probabilities = c })
    (Bin.triple (Bin.array codec_kind) Bin.int (Bin.list Bin.int))

type dropset = [ `Nice | `Classic ]

let of_dropset = function
  | `Nice -> nice
  | `Classic -> classic
