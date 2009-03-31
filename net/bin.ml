exception End_of_buffer

module Buf: sig
  type t
  val make: unit -> t
  val seek: t -> int -> unit
  val read_char: t -> char
  val write_char: t -> char -> unit
end = struct
  type t = {
    mutable size: int;
    mutable buf: string;
    mutable pos: int;
  }

  let make () = {
    size = 32;
    buf = String.create 32;
    pos = 0;
  }

  let extend_if_needed buf =
    while buf.pos >= buf.size do
      let newsize = buf.size * 2 in
      let newbuf = String.create newsize in
      String.blit buf.buf 0 newbuf 0 buf.size;
      buf.buf <- newbuf;
      buf.size <- newsize;
    done

  let seek buf pos =
    buf.pos <- pos;
    extend_if_needed buf

  let read_char buf =
    if buf.pos >= 0 && buf.pos < buf.size then begin
      let c = buf.buf.[buf.pos] in
      buf.pos <- buf.pos + 1;
      c
    end else
      raise End_of_buffer

  let write_char buf c =
    extend_if_needed buf;
    buf.buf.[buf.pos] <- c;
    buf.pos <- buf.pos + 1
end

(******************************************************************************)

type 'a t = {
  enc: Buf.t -> 'a -> unit;
  dec: Buf.t -> 'a;
}

let of_string = assert false

let of_buffer = assert false

let of_channel = assert false

let to_buffer = assert false

let to_channel = assert false

let to_string = assert false

let to_my_string = assert false

(******************************************************************************)

let char =
  {
    enc = Buf.write_char;
    dec = Buf.read_char;
  }

let bool = assert false

let int = assert false

let string = assert false

let couple a b =
  {
    enc = (fun buf (x, y) -> a.enc buf x; b.enc buf y);
    dec = (fun buf -> a.dec buf, b.dec buf);
  }

let list ?min ?max a = assert false

let custom enc dec a =
  {
    enc = (fun buf x -> a.enc buf (enc x));
    dec = (fun buf -> dec (a.dec buf));
  }
