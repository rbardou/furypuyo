type t =
  | List1 of (int * int * Puyo.t) list
  | List2 of (int * int * Puyo.t) list
  | Quad of Puyo.color * Puyo.color list

let rotate_left = function
  | List1 l ->
      List1 (List.map (fun (x, y, p) -> y, -x, p) l)
  | List2 l ->
      List2 (List.map (fun (x, y, p) -> y, -x+1, p) l)
  | Quad (x, l) ->
      match List.rev l with
        | [] -> Quad (x, [])
        | y::r -> Quad (y, x :: List.rev r)

let rotate_right = function
  | List1 l ->
      List1 (List.map (fun (x, y, p) -> -y, x, p) l)
  | List2 l ->
      List2 (List.map (fun (x, y, p) -> -y+1, x, p) l)
  | Quad (x, l) ->
      match l with
        | [] -> Quad (x, [])
        | y::r -> Quad (y, r @ [x])
