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

open Cell

type t =
  | List0 of (int * int * Puyo.t) list
  | List1 of (int * int * Puyo.t) list
  | List2 of (int * int * Puyo.t) list
  | Quad of Puyo.color * Puyo.color list

let rotate_left = function
  | List0 l ->
      List0 (List.map (fun (x, y, p) -> y-1, 1-x, p) l)
  | List1 l ->
      List1 (List.map (fun (x, y, p) -> y, -x, p) l)
  | List2 l ->
      List2 (List.map (fun (x, y, p) -> y, -x+1, p) l)
  | Quad (x, l) ->
      match List.rev l with
        | [] -> Quad (x, [])
        | y::r -> Quad (y, x :: List.rev r)

let rotate_right = function
  | List0 l ->
      List0 (List.map (fun (x, y, p) -> 1-y, 1+x, p) l)
  | List1 l ->
      List1 (List.map (fun (x, y, p) -> -y, x, p) l)
  | List2 l ->
      List2 (List.map (fun (x, y, p) -> -y+1, x, p) l)
  | Quad (x, l) ->
      match l with
        | [] -> Quad (x, [])
        | y::r -> Quad (y, r @ [x])

let collision block x y matrix =
  let ok x' y' =
    let x = x+x' and y = y+y' in
    Matrix.inside matrix x y && (Matrix.get matrix x y).puyo = None
  in
  let ko x' y' = not (ok x' y') in
  match block with
    | List0 l
    | List1 l
    | List2 l ->
        List.fold_left
          (fun acc (x, y, _) -> acc || ko x y)
          false l
    | Quad _ ->
        ko 0 0 || ko 1 0 || ko 0 1 || ko 1 1

let insert x y matrix (x', y', p) =
  Matrix.set matrix (x+x') (y+y') (Cell.make p)

let insert_list matrix x y l =
  List.fold_left (insert x y) matrix l

let insert block x y matrix =
  match block with
    | List0 l
    | List1 l
    | List2 l ->
        insert_list matrix x y l
    | Quad (c, _) ->
        let p = Puyo.make c in
        insert_list matrix x y [ 0, 0, p; 0, 1, p; 1, 0, p; 1, 1, p ]
