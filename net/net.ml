type channel_kind =
  | Fast
  | FastOrdered
  | Important
  | Ordered

module type PROTOCOL = sig
  type message
  val channel: message -> int
  val channels: (int * channel_kind) list
end

module type SIMPLEPROTOCOL = sig
  type message
  val kind: channel_kind
end

module type NET = sig
  type message
  type connection
  type server
  val listen: ?addr: string list -> int -> server
  val accept: ?max: int -> server -> connection list
  val connect: string -> int -> connection
  val close: connection -> unit
  val stop: server -> unit
  val ready: connection -> bool
  val active: connection -> bool
  val send: connection -> message -> unit
  val receive: connection -> message list
  val remote_address: connection -> string
  val remote_port: connection -> int
end

module Make(P: PROTOCOL): NET with type message = P.message = struct
  type message = P.message

  type frame =
    | FFast of message Frame.frame
    | FFastOrdered of message Frame.frame * message Order.orderer
    | FImportant of message Frame.frame * message Resend.sender
    | FOrdered of message Frame.frame * message Resend.sender
        * message Order.orderer

  type connection = {
    connection: message Frame.m Channel.m Connect.connection;
    frames: (int * frame) list;
  }

  type server = message Frame.m Channel.m Connect.server

  let listen ?addr port = Connect.listen ?addr port

  let make_connection cx =
    let frames =
      List.map
        (fun (ch, kind) ->
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
           ch, frame)
        P.channels
    in
    {
      connection = cx;
      frames = frames;
    }

  let accept ?max (serv: server) =
    List.map make_connection (Connect.accept ?max serv)

  let connect addr port = make_connection (Connect.connect addr port)

  let close cx = Connect.close cx.connection

  let stop serv = Connect.stop serv

  let ready cx = Connect.ready cx.connection

  let active cx = Connect.active cx.connection

  let remote_address cx = Connect.remote_address cx.connection

  let remote_port cx = Connect.remote_port cx.connection

  let frame cx msg = List.assoc (P.channel msg) cx.frames

  let update_frame = function
    | FFast _
    | FFastOrdered (_, _) ->
        ()
    | FImportant (_, sender)
    | FOrdered (_, sender, _) ->
        Resend.update sender

  let update cx =
    List.iter (fun (_, frame) -> update_frame frame) cx.frames

  let send cx msg =
    update cx;
    match frame cx msg with
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

  let receive cx =
    update cx;
    List.flatten (List.map (fun (_, frame) -> receive_on frame) cx.frames)
end

module SimpleDef(P: SIMPLEPROTOCOL): PROTOCOL with type message = P.message =
struct
  type message = P.message
  let channel _ = 0
  let channels = [ 0, P.kind ]
end

module Simple(P: SIMPLEPROTOCOL): NET with type message = P.message =
  Make(SimpleDef(P))
