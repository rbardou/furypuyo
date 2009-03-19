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

module type HIGHSCORES = sig
  type score
  type t
  val load: int -> string -> t
  val save: t -> unit
  val add: t -> string -> score -> t * bool
  val player: t -> string -> score list
  val all_players: t -> (string * score list) list
  val top: ?plimit: int -> ?size: int -> t -> (string * score) list
end

module type SCORE = sig
  type t
  val compare: t -> t -> int
end

module StringMap = Map.Make(String)

module Make(C: SCORE) = struct
  type score = C.t

  type t = {
    size: int;
    file: string;
    players: (score list) StringMap.t;
  }

  let load size file =
    if Sys.file_exists (Config.filename file) then begin
      let ch = Config.open_in file in
      let res = (Marshal.from_channel ch: t) in
      close_in ch;
      res
    end else {
      size = size;
      file = file;
      players = StringMap.empty;
    }

  let save (h: t) =
    let ch = Config.open_out h.file in
    Marshal.to_channel ch h [];
    close_out ch

  let add h name score =
    let scores =
      try
        StringMap.find name h.players
      with Not_found ->
        []
    in
    if List.length scores >= h.size &&
      C.compare (list_last scores) score > 0 then
        h, false
    else
      let scores = score :: scores in
      let scores = List.sort (fun x y -> - C.compare x y) scores in
      let scores = list_trunc scores h.size in
      { h with players = StringMap.add name scores h.players }, true

  let player h name =
    try
      StringMap.find name h.players
    with Not_found ->
      []

  let all_players h =
    StringMap.fold
      (fun name scores acc -> (name, scores) :: acc)
      h.players
      []

  let top ?plimit ?size h =
    let scores =
      StringMap.fold
        (fun name pscores scores ->
           let pscores = match plimit with
             | None -> pscores
             | Some plimit -> list_trunc pscores plimit
           in
           List.map (fun x -> name, x) pscores @ scores)
        h.players
        []
    in
    let scores = List.sort (fun (_, x) (_, y) -> - C.compare x y) scores in
    let scores = match size with
      | None -> scores
      | Some size -> list_trunc scores size
    in
    scores
end
