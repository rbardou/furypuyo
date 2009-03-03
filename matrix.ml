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
