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

(** Synchronization of multiplayer games *)

(** To view the field of other players, we collect all their input information
    and advance their copy of the game only when we are sure that we won't
    receive any input from the past.

    So we keep a copy of their game, the last safe time
    (the time of the last received input unless we receive a SafeTime message),
    and the list of all future inputs.

    This allows us not to have to backtrack, which would be possible but more
    time-consuming.

    However, if our copy of their game lags too much w.r.t. the safe time, we
    accelerate their game to try to keep up. *)

type t
  (** The type of a synchronized game, i.e. a player view. *)

val create: Generator.t -> Rand.t -> t
  (** Create a new game which will be synchronized with a given player view,
      from the player's generators. *)

val game: t -> Game.game
  (** Get the current game. *)

val step: t -> t
  (** Advance a step if possible. *)

val inputs: t -> int -> Action.t list -> t
  (** [inputs g t l]: inputs [l] will be executed when computing frame [t+1],
      i.e. the frame that will lead to [(game g).now = t+1]. They are the
      actions received between [t] and [t+1].

      [t] must be strictly greater than the previous [t] given to [inputs]. *)
