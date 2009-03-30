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
