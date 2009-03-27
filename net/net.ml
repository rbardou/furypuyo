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

  type connection = message Channel.m Connect.connection

  type server = message Channel.m Connect.server

  let listen ?addr port = Connect.listen ?addr port

  let accept ?max serv = Connect.accept ?max serv

  let connect addr port = Connect.connect addr port

  let close cx = Connect.close cx

  let stop serv = Connect.stop serv

  let ready cx = Connect.ready cx

  let active cx = Connect.active cx

  let channel cx msg = Channel.channel cx (P.channel msg)

  let send cx msg = Channel.send (channel cx msg) msg

  let receive cx = List.map snd (Channel.receive_all cx)

  let remote_address cx = Connect.remote_address cx

  let remote_port cx = Connect.remote_port cx
end

module SimpleDef(P: SIMPLEPROTOCOL): PROTOCOL with type message = P.message =
struct
  type message = P.message
  let channel _ = 0
  let channels = [ 0, P.kind ]
end

module Simple(P: SIMPLEPROTOCOL): NET with type message = P.message =
  Make(SimpleDef(P))
