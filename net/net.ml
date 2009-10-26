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

type channel_kind =
  | Fast
  | FastOrdered
  | Important
  | Ordered

module type PROTOCOL = sig
  type message
  val channel: message -> int
  val channels: (int * channel_kind) list
  val codec: message Bin.t
end

module type SIMPLEPROTOCOL = sig
  type message
  val kind: channel_kind
  val codec: message Bin.t
end

module type NET = sig
  type message_to_server
  type message_to_client
  type ('a, 'b) connection
  type server
  val listen: ?addr: string list -> int -> server
  val accept: ?max: int -> server ->
    (message_to_client, message_to_server) connection list
  val connect: string -> int ->
    (message_to_server, message_to_client) connection
  val close: ('a, 'b) connection -> unit
  val stop: server -> unit
  val ready: ('a, 'b) connection -> bool
  val active: ('a, 'b) connection -> bool
  val send: ('a, 'b) connection -> 'a -> unit
  val receive: ('a, 'b) connection -> 'b list
  val receive_one: ('a, 'b) connection -> 'b option
  val remote_address: ('a, 'b) connection -> string
  val remote_port: ('a, 'b) connection -> int
end

module Make(ToServer: PROTOCOL)(ToClient: PROTOCOL): NET
  with type message_to_server = ToServer.message
  with type message_to_client = ToClient.message =
struct
  type message_to_server = ToServer.message
  type message_to_client = ToClient.message

  type ('a, 'b) frame =
    | FFast of ('a, 'b) Frame.frame
    | FFastOrdered of ('a, 'b) Frame.frame * 'b Order.orderer
    | FImportant of ('a, 'b) Frame.frame * 'a Resend.sender
    | FOrdered of ('a, 'b) Frame.frame * 'a Resend.sender
        * 'b Order.orderer

  type ('a, 'b) connection = {
    is_server: bool;
    connection: ('a Frame.m Channel.m, 'b Frame.m Channel.m) Connect.connection;
    frames_send: (int * ('a, 'b) frame) list;
    frames_receive: (int * ('a, 'b) frame) list;
    channel_send: 'a -> int;
    channel_receive: 'b -> int;
    mutable reception: 'b list;
  }

  type server =
      (message_to_client Frame.m Channel.m,
       message_to_server Frame.m Channel.m) Connect.server

  let codec_to_server = Channel.codec (Frame.codec ToServer.codec)
  let codec_to_client = Channel.codec (Frame.codec ToClient.codec)

  let listen ?addr port =
    Connect.listen ?addr port codec_to_client codec_to_server

  let real_channel ch mode is_server =
    match mode, is_server with
      | `S, true | `R, false -> 2 * ch
      | `R, true | `S, false -> 2 * ch + 1

  let make_frame is_server mode cx (ch, kind) =
    let ch = real_channel ch mode is_server in
    let frame = Frame.start (Channel.channel cx ch) in
    let frame =
      match kind with
        | Fast ->
            FFast frame
        | FastOrdered ->
            let receiver = Order.start ~size: 0 frame in
            FFastOrdered (frame, receiver)
        | Important ->
            let sender = Resend.start frame in
            FImportant (frame, sender)
        | Ordered ->
            let sender = Resend.start frame in
            let receiver = Order.start frame in
            FOrdered (frame, sender, receiver)
    in
    ch, frame

  let make_connection is_server channels_send channels_receive
      ch_send ch_receive cx =
    {
      is_server = is_server;
      connection = cx;
      frames_send = List.map (make_frame is_server `S cx) channels_send;
      frames_receive = List.map (make_frame is_server `R cx) channels_receive;
      channel_send = ch_send;
      channel_receive = ch_receive;
      reception = [];
    }

  let accept ?max (serv: server) =
    let make_connection =
      make_connection
        true
        ToClient.channels
        ToServer.channels
        ToClient.channel
        ToServer.channel
    in
    List.map make_connection (Connect.accept ?max serv)

  let connect addr port =
    make_connection
      false
      ToServer.channels
      ToClient.channels
      ToServer.channel      
      ToClient.channel
      (Connect.connect addr port codec_to_server codec_to_client)

  let close cx = Connect.close cx.connection

  let stop serv = Connect.stop serv

  let ready cx = Connect.ready cx.connection

  let active cx = Connect.active cx.connection

  let remote_address cx = Connect.remote_address cx.connection

  let remote_port cx = Connect.remote_port cx.connection

  let frame_send cx msg =
    List.assoc
      (real_channel (cx.channel_send msg) `S cx.is_server)
      cx.frames_send

  let frame_receive cx msg =
    List.assoc
      (real_channel (cx.channel_receive msg) `R cx.is_server)
      cx.frames_receive

  let update_frame = function
    | FFast _
    | FFastOrdered (_, _) ->
        ()
    | FImportant (_, sender)
    | FOrdered (_, sender, _) ->
        Resend.update sender

  let update cx =
    List.iter (fun (_, frame) -> update_frame frame) cx.frames_send;
    List.iter (fun (_, frame) -> update_frame frame) cx.frames_receive

  let send cx msg =
    update cx;
    match frame_send cx msg with
      | FFast frame
      | FFastOrdered (frame, _) ->
          Frame.send frame msg;
          Frame.shift frame (Frame.next frame)
      | FImportant (_, sender)
      | FOrdered (_, sender, _) ->
          Resend.send sender msg

  let receive_on = function
    | FFast frame
    | FImportant (frame, _) ->
        List.map snd (Frame.receive frame)
    | FFastOrdered (_, receiver)
    | FOrdered (_, _, receiver) ->
        Order.receive receiver

  let reception cx =
    update cx;
    let news =
      List.flatten
	(List.map (fun (_, frame) -> receive_on frame) cx.frames_receive)
    in
    cx.reception <- cx.reception @ news

  let receive cx =
    reception cx;
    let l = cx.reception in
    cx.reception <- [];
    l

  let receive_one cx =
    reception cx;
    match cx.reception with
      | [] -> None
      | x :: r -> cx.reception <- r; Some x
end

module SimpleDef(P: SIMPLEPROTOCOL): PROTOCOL
  with type message = P.message =
struct
  type message = P.message
  let channel _ = 0
  let channels = [ 0, P.kind ]
  let codec = P.codec
end

module Simple(ToServer: SIMPLEPROTOCOL)(ToClient: SIMPLEPROTOCOL): NET
  with type message_to_server = ToServer.message
  with type message_to_client = ToClient.message =
  Make(SimpleDef(ToServer))(SimpleDef(ToClient))
