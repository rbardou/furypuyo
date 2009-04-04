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

type state =
  | Young
  | InFrame
  | Old

module F: sig
  type 'a t
  val make: int -> 'a -> 'a -> 'a t (* size, young / in default, old default *)
  val get: 'a t -> int -> 'a (* unspecified if out of frame *)
  val set: 'a t -> int -> 'a -> unit (* same *)
  val shift: 'a t -> int -> unit (* only applied if move forward *)
  val position: 'a t -> int
  val state: 'a t -> int -> state
  val size: 'a t -> int
end = struct
  type 'a t = {
    size: int;
    array: 'a array;
    young: 'a;
    old: 'a;
    mutable position: int;
  }

  let make size young old = {
    size = size;
    array = Array.make size young;
    young = young;
    old = old;
    position = 0;
  }

  let state frame pos =
    if pos < frame.position then Old else
      if pos >= frame.position + frame.size then Young else
        InFrame

  let in_frame frame pos =
    state frame pos = InFrame

  let apos frame pos =
    (pos - frame.position) mod frame.size

  let get frame pos =
    match state frame pos with
      | Young -> frame.young
      | InFrame -> frame.array.(apos frame pos)
      | Old -> frame.old

  let set frame pos v =
    if in_frame frame pos then
      frame.array.(apos frame pos) <- v

  let shift frame pos =
    if pos > frame.position then begin
      for i = frame.position to pos - 1 do
        set frame i frame.young
      done;
      frame.position <- pos
    end

  let position frame =
    frame.position

  let size frame =
    frame.size
end
