type t =
  | List of (int * int * Puyo.t) list
  | Quad of Puyo.color * Puyo.color list

let rotate_left = function
  | List l ->
      List (List.map (fun (x, y, p) -> -y, x, p) l)
  | Quad (x, l) ->
      match List.rev l with
        | [] -> Quad (x, [])
        | y::r -> Quad (y, x :: List.rev r)

let rotate_right = function
  | List l ->
      List (List.map (fun (x, y, p) -> y, -x, p) l)
  | Quad (x, l) ->
      match l with
        | [] -> Quad (x, [])
        | y::r -> Quad (y, r @ [x])
