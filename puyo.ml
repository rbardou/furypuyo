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

type color = Red | Green | Blue | Yellow | Gray

type moving_effect = {
  me_start: int;
  me_sx: float; (** cell per tick *)
  me_sy: float;
  me_px: float; (** cell *)
  me_py: float;
  me_end: int;
}

type effect =
  | NoEffect
  | Moving of moving_effect

type t = {
  color: color;
  effect: effect;
  (* mimic: ... *)
}

let moving_effect now x y delay =
  let x = float_of_int x in
  let y = float_of_int y in
  let fdelay = if delay = 0 then 1. else float_of_int delay in
  Moving {
    me_start = now;
    me_sx = x /. fdelay;
    me_sy = y /. fdelay;
    me_px = -.x;
    me_py = -.y;
    me_end = now + delay;
  }

let make color = {
  color = color;
  effect = NoEffect;
}

let gray = make Gray

let to_string p =
  match p.color with
    | Red -> "Red"
    | Green -> "Green"
    | Blue -> "Blue"
    | Yellow -> "Yellow"
    | Gray -> "Gray"
