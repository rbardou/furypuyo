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

(** In-game menus *)

val string_choices: ?default: 'a -> (string * 'a) list -> 'a
  (** The user chooses from a vertical, centered list of strings.

      @param default value to return if the escape key is pressed *)

val input_string: ?default: string -> ?passchar: char -> string -> string
  (** The user inputs a string.

      @param default default value
      @param passchar replace characters by this password char (ex: ['*'])

      The other parameter is the input query. *)

val show_high_scores: (string * (string list)) list -> unit
  (** Show high scores.

      The argument is a list of high score pages, with a title and a list
      of lines to be printed. The user can switch between pages. *)

val waiting_string: string -> (unit -> 'a option) -> 'a
  (** Show a waiting string with a message.

      [waiting_string message f]: show [message] until the user hits
      the escape key or [f ()] returns [Some x]. If the user has hit the escape
      key, exception [Exit] is raised instead of returning normally.
      If [f ()] returns [Some x], this [x] value is returned. *)

val show_message: string -> unit
  (** Show a message to the user.

      [show_message message]: show [message] until the user hits either escape
      or return. *)

type page

val draw_high_scores_page: page -> unit

val high_scores_top_players_page: ?pos: int -> (string * Score.t) list -> page

val high_scores_player_page: string -> Score.t list -> page
