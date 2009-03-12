type t =
  | ClearScreen

(* O(1) concatainable lists *)
type 'a clist =
  | Empty
  | Item of 'a
  | Concat of 'a clist * 'a clist

module IntMap = Map.Make (struct type t = int let compare = compare end)

type set = t clist IntMap.t

let empty = IntMap.empty

let add set effect ending =
  let previous = try
    IntMap.find ending set
  with Not_found ->
    Empty
  in
  IntMap.add ending (Concat (previous, Item effect)) set

let remove set now =
  IntMap.remove now set

let rec clist_iter f = function
  | Empty -> ()
  | Item x -> f x
  | Concat (l, r) -> clist_iter f l; clist_iter f r

let iter f = IntMap.iter (fun _ -> clist_iter f)

let rec clist_map f = function
  | Empty -> Empty
  | Item x -> Item (f x)
  | Concat (l, r) -> Concat (clist_map f l, clist_map f r)

let map f = IntMap.map (clist_map f)
