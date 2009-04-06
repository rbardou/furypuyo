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

open Netmisc

type 'a orderer = {
  frame_receive: (unit -> (int * 'a) list);
  mutable next_deliverable: int;
  mutable buffer_before: (int * 'a) list;
  mutable buffer: 'a option F.t;
}

let start ?size frame =
  let size =
    match size with
      | None ->
          Frame.size frame
      | Some size ->
          if size <= 0 then 0 else size
  in
  {
    frame_receive = (fun () -> Frame.receive frame);
    next_deliverable = 0;
    buffer_before = [];
    buffer = F.make size None None;
  }

let rec pop_some acc id buf =
  match F.get buf id with
    | None ->
        acc
    | Some msg ->
        F.shift buf (id + 1);
        pop_some ((id, msg) :: acc) (id + 1) buf

let pop_some buf = pop_some [] (F.position buf) buf

let rec shift_some acc buf n =
  if n <= 0 then
    acc
  else
    let acc =
      let id = F.position buf in
      match F.get buf id with
        | None -> acc
        | Some msg -> (id, msg) :: acc
    in
    shift_some acc buf (n - 1)

let shift_some buf n = shift_some [] buf n

let message ord (id, msg) =
  if id >= ord.next_deliverable then begin
    match F.state ord.buffer id with
      | Young ->
          let shift = id - F.position ord.buffer - F.size ord.buffer + 1 in
          ord.buffer_before <- ord.buffer_before @ shift_some ord.buffer shift;
          F.set ord.buffer id (Some msg);
          ord.buffer_before <- ord.buffer_before @ pop_some ord.buffer
      | InFrame ->
          F.set ord.buffer id (Some msg);
          ord.buffer_before <- ord.buffer_before @ pop_some ord.buffer
      | Old ->
          ord.buffer_before <- (id, msg) :: ord.buffer_before
  end

let update ord =
  List.iter (message ord) (ord.frame_receive ())

let receive ord =
  update ord;
  let b = ord.buffer_before in
  ord.buffer_before <- [];
  List.map snd (List.sort (fun (a, _) (b, _) -> compare a b) b)
