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

(** Miscellaneous stuff *)

open Printf
open Unix

let rec list_mapi acc i f = function
  | [] -> List.rev acc
  | x :: rem -> list_mapi (f i x :: acc) (i + 1) f rem
let list_mapi f l = list_mapi [] 0 f l

let rec list_iteri i f = function
  | [] -> ()
  | x :: rem ->
      f i x;
      list_iteri (i + 1) f rem
let list_iteri f l = list_iteri 0 f l

let rec split_when f ?(acc = []) = function
  | [] -> raise Not_found
  | x :: r ->
      if f x then List.rev acc, x, r else
        let acc = x :: acc in
        split_when f ~acc r

let rec list_last = function
  | [] -> raise (Invalid_argument "list_last")
  | [ x ] -> x
  | _ :: rem -> list_last rem

let rec list_trunc acc l = function
  | 0 -> List.rev acc
  | n ->
      match l with
        | [] -> List.rev acc
        | x :: rem ->
            list_trunc (x :: acc) rem (n - 1)
let list_trunc x = list_trunc [] x

let rec new_file_name i base ext =
  let name =
    if i = 0 then base ^ ext
    else Printf.sprintf "%s_%d%s" base i ext
  in
  if Sys.file_exists name then
    new_file_name (i + 1) base ext
  else
    name
let new_file_name = new_file_name 0

let log x =
  ksprintf
    (fun s ->
       let time = localtime (time ()) in
       printf "[%02d:%02d:%02d] %s\n%!"
         time.tm_hour time.tm_min time.tm_sec s)
    x
