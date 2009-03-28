type 'a m =
  | Message of int * 'a
  | Acknowledge of int

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
end

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

let send ?(ack = fun () -> ()) ?id frame msg =
  let id =
    match id with
      | None ->
          let id = frame.next in
          frame.next <- frame.next + 1;
          id
      | Some id ->
          id
  in
  frame.send_buffer <- (id, msg, ack) :: frame.send_buffer;
  update frame;
  id

let receive frame =
  update frame;
  let list = frame.recv_buffer in
  frame.recv_buffer <- [];
  List.sort (fun (a, _) (b, _) -> compare a b) list
