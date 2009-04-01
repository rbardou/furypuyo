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

(** High scores loading, updating, and saving *)

exception Cannot_read_scores of string
  (** The argument is the reason. *)

exception Cannot_write_scores of string
  (** The argument is the reason. *)

module type HIGHSCORES = sig
  type score
    (** Score associated to a player. *)

  type t
    (** The persistent type of high score records. *)

  val codec: t Bin.t

  val empty: int -> t
    (** Make an empty high scores table.

        [empty size]: make an empty high scores table.
        The maximum number of records for one player is
        [size], but any number of players can be recorded. *)

  val load: string -> int -> t
    (** Load a high scores table from a file.

        [load file size]: if [file] exists, read an high scores table from it.
        May raise [Cannot_read_scores]. Else, return an empty table of size
        [size]. *)

  val save: t -> string -> unit
    (** Save a high scores table to a file.

        [save table file]: save [table] to [file].

        May raise [Cannot_write_scores]. *)

  val add: t -> string -> score -> t * bool
    (** Add a new score.

        [add h player score]: add score [score] for player [player] in high
        scores record [h]. Return [h2, changed] where [h2] is the new high
        scores table and [changed] is [true] if the table actually changed
        (i.e. the score was a good one). *)

  val player: t -> string -> score list
    (** Get the high scores of a player.

        [player h p]: get the high scores of player [p] in table [h]. The list
        length is at most the size given to [load]. The list is sorted from
        the highest score to the lowest. *)

  val all_players: t -> (string * score list) list
    (** Get the high scores of all players.

        Same as [player] but return the high scores of all players. *)

  val top: ?plimit: int -> ?size: int -> t -> (string * score) list
    (** Get the high scores of all players, merged together.

        The list is sorted from the highest score to the lowest.

        @param size if given, the resulting list length cannot exceed [size].
        @param plimit if given, a player cannot appear more than [plimit] times
        in the resulting list. *)
end

module type SCORE = sig
  type t
  val compare: t -> t -> int
  val codec: t Bin.t
end

module Make(C: SCORE): HIGHSCORES with type score = C.t
