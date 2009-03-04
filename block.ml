open Cell

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

let collision block x y matrix =
  let ok x' y' =
    let x = x+x' and y = y+y' in
    Matrix.inside matrix x y && (Matrix.get matrix x y).puyo = None
  in
  let ko x' y' = not (ok x' y') in
  match block with
    | List1 l
    | List2 l ->
        List.fold_left
          (fun acc (x, y, _) -> acc || ko x y)
          false l
    | Quad _ ->
        ko 0 0 || ko 1 0 || ko 0 1 || ko 1 1
