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

type particle_sprite =
  | GreenStar
  | YellowStar
  | RedStar
  | PurpleStar

type particle = {
  sprite: particle_sprite;
  cx: int;
  cy: int;
  mutable x: float;
  mutable y: float;
  mutable vx: float;
  mutable vy: float;
  ax: float;
  ay: float;
}

type t =
  | ClearScreen
  | Particle of particle

(* O(1) concatainable lists *)
type 'a clist =
  | Empty
  | Item of 'a
  | Concat of 'a clist * 'a clist

module IntMap = Map.Make (struct type t = int let compare = compare end)

type set = t clist IntMap.t

let empty = IntMap.empty

let add set effect ending =
  let previous = try
    IntMap.find ending set
  with Not_found ->
    Empty
  in
  IntMap.add ending (Concat (previous, Item effect)) set

let remove set now =
  IntMap.remove now set

let rec clist_iter f = function
  | Empty -> ()
  | Item x -> f x
  | Concat (l, r) -> clist_iter f l; clist_iter f r

let iter f = IntMap.iter (fun _ -> clist_iter f)

let rec clist_map f = function
  | Empty -> Empty
  | Item x -> Item (f x)
  | Concat (l, r) -> Concat (clist_map f l, clist_map f r)

let map f = IntMap.map (clist_map f)
