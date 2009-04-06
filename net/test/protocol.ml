type message =
  | Unit
  | String of string
  | Int of int
  | Bool of bool

type channel = int

let channels = [
  0, Net.Fast;
  1, Net.FastOrdered;
  2, Net.Important;
  3, Net.Ordered;
]

let channel = function
  | Unit -> 0
  | Bool _ -> 1
  | String _ -> 2
  | Int _ -> 3

let encode buf = function
  | Unit ->
      Bin.write buf Bin.int 0
  | String s ->
      Bin.write buf Bin.int 1;
      Bin.write buf Bin.string s
  | Int i ->
      Bin.write buf Bin.int 2;
      Bin.write buf Bin.int i
  | Bool b ->
      Bin.write buf Bin.int 3;
      Bin.write buf Bin.bool b

let decode buf =
  match Bin.read buf Bin.int with
    | 0 -> Unit
    | 1 -> String (Bin.read buf Bin.string)
    | 2 -> Int (Bin.read buf Bin.int)
    | 3 -> Bool (Bin.read buf Bin.bool)
    | _ -> failwith "test protocol decoder"

let codec =
  Bin.custom encode decode
