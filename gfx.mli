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

(** Graphic effects *)

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
  | ClearScreen (** Player cleared screen. *)
  | Particle of particle (** Particle effect. *)
  | Chain of int * float * float (** Chain count (count, cell x, cell y). *)
  (** Graphic effect kinds. *)

type set
  (** Graphic effect set. *)

val empty: set
  (** Empty graphic effect set. *)

val add: set -> t -> int -> set
  (** Add an effect to a set.

[add set effect ending]: add [effect] to [set]. The effect will be removed
after an [update] with time greater or equal than [ending]. *)

val remove: set -> int -> set
  (** Remove effects of a given ending time.

[remove set now]: remove effects whose ending time is [now]. Effects whose
ending time is strictly less than [now] are not removed. *)

val iter: (t -> unit) -> set -> unit
  (** Iteration over a set of graphic effects. *)

val map: (t -> t) -> set -> set
  (** Map a function to the effects of a set. *)
