open Block
open Puyo

type state = int

type t = {
  generator: t -> Rand.t -> Rand.t * t * Block.t;
  state: int;
}

let combinations l =
  List.flatten (List.map (fun x -> List.map (fun y -> x, y) l) l)

let next_color4 = function
  | Red -> Green
  | Green -> Blue
  | Blue -> Yellow
  | Yellow -> Red

let next_colors f origin =
  let rec next acc color =
    let nc = f color in
    if nc = origin then acc else next (nc :: acc) nc
  in
  List.rev (next [] origin)

let next_in_list l c =
  match l with
    | [] -> c
    | hd::tl ->
        let rec find = function
          | [] | [_] -> hd
          | x::y::r -> if x = c then y else find (y::r)
        in
        find l

let twos colors =
  List.map
    (fun (a, b) -> List1 [ 0, 0, Puyo.make a;
                           0, 1, Puyo.make b ])
    (combinations colors)

let threes1 colors =
  List.map
    (fun (a, b) -> List1 [ 0, 0, Puyo.make a;
                           0, 1, Puyo.make a;
                           1, 0, Puyo.make b ])
    (combinations colors)

let threes2 colors =
  List.map
    (fun (a, b) -> List1 [ 0, 0, Puyo.make a;
                           0, 1, Puyo.make a;
                           1, 1, Puyo.make b ])
    (combinations colors)

let twotwos colors =
  let combs = combinations colors in
  let combs = List.filter (fun (a, b) -> a <> b) combs in
  List.map
    (fun (a, b) -> List2 [ 0, 0, Puyo.make a;
                           0, 1, Puyo.make a;
                           1, 0, Puyo.make b;
                           1, 1, Puyo.make b ])
    combs

let quads colors =
  List.map (fun a -> Quad (a, next_colors (next_in_list colors) a)) colors

let all_blocks colors =
  twos colors @ threes1 colors @ threes2 colors @ twotwos colors @ quads colors

let random_from blocks =
  let blocks = Array.of_list blocks in
  let count = Array.length blocks in
  let gen state rand =
    let rand, n = Rand.int rand count in
    rand, state, blocks.(n)
  in {
    generator = gen;
    state = 0;
  }

let random colors =
  random_from (all_blocks colors)

let only_twos colors =
  random_from (twos colors)

let sequence l =
  let blocks = Array.of_list (List.map Array.of_list l) in
  let gen state rand =
    let pos = state.state in
    let pos = if pos < 0 || pos >= Array.length blocks then 0 else pos in
    let blocks = blocks.(pos) in
    let rand, n = Rand.int rand (Array.length blocks) in
    rand, { state with state = pos + 1 }, blocks.(n)
  in {
    generator = gen;
    state = 0;
  }

let nice colors =
  sequence [
    twos colors;
    twos colors;
    twos colors;
    quads colors @ twotwos colors;
    twos colors;
    threes1 colors @ threes2 colors;
    twos colors;
    twos colors @ threes1 colors @ twotwos colors;
  ]

let next gen rand = gen.generator gen rand
