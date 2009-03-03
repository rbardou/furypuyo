module Field: sig
  (** Semi-persistent matrix *)
  type 'a t
  val make: int -> int -> 'a -> 'a t
  val get: 'a t -> int -> int -> 'a
  val set: 'a t -> int -> int -> 'a -> 'a t
  val width: 'a t -> int
  val height: 'a t -> int
end = struct
  module PosMap = Map.Make(struct type t = int * int let compare = compare end)

  type 'a data = 'a PosMap.t

  type 'a t = {
    width: int;
    height: int;
    default: 'a;
    data: 'a data;
  }

  let make w h v = {
    width = w;
    height = h;
    default = v;
    data = PosMap.empty;
  }

  let get f x y = try PosMap.find (x, y) f.data with Not_found -> f.default

  let set f x y v = { f with data = PosMap.add (x, y) v f.data }

  let width f = f.width

  let height f = f.height
end

type puyo_color = Red | Green | Blue | Yellow

type puyo = {
  color: puyo_color;
  (* mimic: ... *)
}

type cell = {
  puyo: puyo option;
  (* animations: ... *)
}

type 'a block = (int * int * 'a) list

type game = {
  field: cell Field.t;
  incoming: puyo block;
}
