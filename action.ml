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

(** Player actions *)

type t =
  | Escape (** escape key (pause menu) *)
  | MLeft
  | MRight
  | MDown
  | MDownRelease
  | RLeft
  | RRight
  | InstaFall

  | SendGarbage of int * int (** player id, garbage count *)
  | FinishGarbage of int (** player id *)

  | ViewOtherPlayer

  | Debug

let encode buf v =
  let w x = Bin.write buf x in
  match v with
    | Escape -> w Bin.int 0
    | MLeft -> w Bin.int 1
    | MRight -> w Bin.int 2
    | MDown -> w Bin.int 3
    | MDownRelease -> w Bin.int 4
    | RLeft -> w Bin.int 5
    | RRight -> w Bin.int 6
    | InstaFall -> w Bin.int 7
    | Debug -> w Bin.int 8
    | SendGarbage (i, j) ->
        w Bin.int 9;
        w Bin.int i;
        w Bin.int j
    | FinishGarbage i ->
        w Bin.int 10;
        w Bin.int i
    | ViewOtherPlayer -> w Bin.int 11

let decode buf =
  let r x = Bin.read buf x in
  match r Bin.int with
    | 0 -> Escape
    | 1 -> MLeft
    | 2 -> MRight
    | 3 -> MDown
    | 4 -> MDownRelease
    | 5 -> RLeft
    | 6 -> RRight
    | 7 -> InstaFall
    | 8 -> Debug
    | 9 ->
        let i = r Bin.int in
        let j = r Bin.int in
        SendGarbage (i, j)
    | 10 -> FinishGarbage (r Bin.int)
    | 11 -> ViewOtherPlayer
    | _ -> failwith "Action.decode"

let codec = Bin.custom encode decode
