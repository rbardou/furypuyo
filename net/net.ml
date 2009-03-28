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

  type connection = {
    connection: message Frame.m Channel.m Connect.connection;
    frames: (int * message Frame.frame) list;
  }

  type server = message Frame.m Channel.m Connect.server

  let listen ?addr port = Connect.listen ?addr port

  let make_connection cx =
    let frames =
      List.map
        (fun (ch, kind) ->
           let frame = Frame.start (Channel.channel cx ch) in
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

  let send cx msg = ignore (Frame.send (frame cx msg) msg)

  let receive cx =
    List.map
      snd
      (List.flatten
         (List.map (fun (_, frame) -> Frame.receive frame) cx.frames))
end

module SimpleDef(P: SIMPLEPROTOCOL): PROTOCOL with type message = P.message =
struct
  type message = P.message
  let channel _ = 0
  let channels = [ 0, P.kind ]
end

module Simple(P: SIMPLEPROTOCOL): NET with type message = P.message =
  Make(SimpleDef(P))
