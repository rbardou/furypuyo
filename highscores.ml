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

exception Cannot_read_scores of string
exception Cannot_write_scores of string

module type HIGHSCORES = sig
  type score
  type t
  val codec: t Bin.t
  val empty: int -> t
  val load: string -> int -> t
  val save: t -> string -> unit
  val add: t -> string -> score -> t * bool
  val player: t -> string -> score list
  val all_players: t -> (string * score list) list
  val top: ?plimit: int -> ?size: int -> t -> (string * score) list
end

module type SCORE = sig
  type t
  val compare: t -> t -> int
  val codec: t Bin.t
end

module StringMap = Map.Make(String)

module Make(C: SCORE) = struct
  type score = C.t

  type scores1 = int * (score list) StringMap.t

  type t =
    | V1 of scores1

  let codec_string_map a =
    Bin.convert
      (fun map -> StringMap.fold (fun k v a -> (k, v) :: a) map [])
      (List.fold_left (fun a (k, v) -> StringMap.add k v a) StringMap.empty)
      (Bin.list (Bin.couple Bin.string a))

  let codec_scores1 =
    Bin.couple Bin.int (codec_string_map (Bin.list C.codec))

  let identifier = Bin.identifier "HSCO"

  let renew = function
    | V1 scores -> V1 scores

  let encode buf scores =
    Bin.write identifier buf ();
    match scores with
      | V1 scores ->
          Bin.write Bin.int buf 1;
          Bin.write codec_scores1 buf scores

  let decode buf =
    Bin.read identifier buf;
    let scores =
      match Bin.read Bin.int buf with
        | 1 -> V1 (Bin.read codec_scores1 buf)
        | n -> raise (Cannot_read_scores ("unknown version: "^string_of_int n))
    in
    renew scores

  let codec =
    Bin.custom encode decode

  let save scores file =
    let file = Config.filename file in
    try
      let ch = open_out file in
      let buf = Bin.to_channel ch in
      Bin.write codec buf scores;
      close_out ch;
    with Sys_error s ->
      raise (Cannot_write_scores ("system error: "^s))

  let empty size =
    V1 (size, StringMap.empty)

  let load file size =
    let file = Config.filename file in
    if Sys.file_exists file then
      try
        let ch = open_in file in
        let buf = Bin.from_channel ch in
        Bin.read codec buf
      with
        | Sys_error s ->
            raise (Cannot_read_scores ("system error: "^s))
        | Bin.Bad_identifier _ ->
            raise (Cannot_read_scores (file ^ ": not a high score file"))
    else
      empty size

  let size = function
    | V1 (size, players) -> size

  let players = function
    | V1 (size, players) -> players

  let set_size h s = match h with
    | V1 (_, players) -> V1 (s, players)

  let set_players h p = match h with
    | V1 (size, _) -> V1 (size, p)

  let add h name score =
    let scores =
      try
        StringMap.find name (players h)
      with Not_found ->
        []
    in
    if List.length scores >= (size h) &&
      C.compare (list_last scores) score > 0 then
        h, false
    else
      let scores = score :: scores in
      let scores = List.sort (fun x y -> - C.compare x y) scores in
      let scores = list_trunc scores (size h) in
      set_players h (StringMap.add name scores (players h)), true

  let player h name =
    try
      StringMap.find name (players h)
    with Not_found ->
      []

  let all_players h =
    StringMap.fold
      (fun name scores acc -> (name, scores) :: acc)
      (players h)
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
        (players h)
        []
    in
    let scores = List.sort (fun (_, x) (_, y) -> - C.compare x y) scores in
    let scores = match size with
      | None -> scores
      | Some size -> list_trunc scores size
    in
    scores
end
