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

let initial_resend_delay = 10
let resend_delay_rate = 2.

type 'a info = {
  msg: 'a;
  mutable last: Time.t;
  mutable delay: Time.d;
}

type 'a sender = {
  frame_next: unit -> int;
  frame_send: ?ack: (unit -> unit) -> 'a -> unit;
  frame_resend: int -> 'a -> unit;
  buffer: (int, 'a info) Hashtbl.t;
}

let start frame =
  {
    frame_next = (fun () -> Frame.next frame);
    frame_send = (fun ?ack -> Frame.send ?ack frame);
    frame_resend = (Frame.resend frame);
    buffer = Hashtbl.create 17;
  }

let send sender msg =
  let id = sender.frame_next () in
  let info =
    {
      msg = msg;
      last = Time.now ();
      delay = Time.ms initial_resend_delay;
    }
  in
  Hashtbl.add sender.buffer id info;
  let ack () = Hashtbl.remove sender.buffer id in
  sender.frame_send ~ack msg

let check_resend frame_resend id info =
  let now = Time.now () in
  if now >= Time.shift info.last info.delay then begin
    info.last <- now;
    info.delay <- Time.multf info.delay resend_delay_rate;
    frame_resend id info.msg
  end

let update sender =
  Hashtbl.iter (check_resend sender.frame_resend) sender.buffer
