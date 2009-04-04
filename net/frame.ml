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

type 'a m =
  | Message of int * 'a
  | Acknowledge of int

type ack_state =
  | NotSent
  | Sent of (unit -> unit)
  | Ack

(* IDs between position and position + size - 1 are valid *)
type 'a frame = {
  channel: 'a m Channel.channel;
  size: int;
  ack: ack_state F.t; (* messages acknowledged by remote peer *)
  received: bool F.t; (* messages we received *)
  mutable next: int; (* next ID to send *)
  mutable recv_buffer: (int * 'a) list; (* to deliver *)
  mutable send_buffer: (int * 'a * (unit -> unit)) list; (* oof to send *)
}

let start ?(size = 100) ch =
  {
    channel = ch;
    size = size;
    ack = F.make size NotSent Ack;
    received = F.make size false true;
    next = 0;
    recv_buffer = [];
    send_buffer = [];
  }

let position frame =
  F.position frame.ack

let shift frame id =
  F.shift frame.ack id

let handle_message frame = function
  | Message (id, msg) ->
      Channel.send frame.channel (Acknowledge id);
      begin match F.state frame.received id with
        | Young ->
            F.shift frame.received (id - frame.size + 1);
            F.set frame.received id true;
            frame.recv_buffer <- (id, msg) :: frame.recv_buffer
        | InFrame ->
            if not (F.get frame.received id) then begin
              F.set frame.received id true;
              frame.recv_buffer <- (id, msg) :: frame.recv_buffer;
            end
        | Old ->
            ()
      end
  | Acknowledge id ->
      match F.get frame.ack id with
        | Sent ackfun ->
            F.set frame.ack id Ack;
            while F.get frame.ack (F.position frame.ack) = Ack do
              F.shift frame.ack (F.position frame.ack + 1)
            done;
            ackfun ()
        | NotSent | Ack ->
            ()

let send_now frame id msg ack =
  F.set frame.ack id (Sent ack);
  Channel.send frame.channel (Message (id, msg))

let update_send_buffer frame =
  let now, later =
    List.partition
      (fun (id, _, _) -> F.state frame.ack id = InFrame)
      frame.send_buffer
  in
  List.iter (fun (id, msg, ack) -> send_now frame id msg ack) now;
  frame.send_buffer <- later

let update frame =
  List.iter (handle_message frame) (Channel.receive frame.channel);
  update_send_buffer frame

let send ?(ack = fun () -> ()) frame msg =
  let id = frame.next in
  frame.next <- frame.next + 1;
  frame.send_buffer <- (id, msg, ack) :: frame.send_buffer;
  update frame

let resend frame id msg =
  Channel.send frame.channel (Message (id, msg))

let receive frame =
  update frame;
  let list = frame.recv_buffer in
  frame.recv_buffer <- [];
  List.sort (fun (a, _) (b, _) -> compare a b) list

let next frame =
  frame.next

let size frame =
  frame.size
