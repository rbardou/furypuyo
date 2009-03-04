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

let all_blocks colors =
  let combs = combinations colors in
  let twos =
    List.map
      (fun (a, b) -> List1 [ 0, 0, Puyo.make a;
                             0, 1, Puyo.make b ])
      combs
  in
  let threes1 =
    List.map
      (fun (a, b) -> List1 [ 0, 0, Puyo.make a;
                             0, 1, Puyo.make a;
                             1, 0, Puyo.make b ])
      combs
  in
  let threes2 =
    List.map
      (fun (a, b) -> List1 [ 0, 0, Puyo.make a;
                             0, 1, Puyo.make a;
                             1, 1, Puyo.make b ])
      combs
  in
  let fours =
    List.map
      (fun (a, b) ->
         if a <> b then
           List2 [ 0, 0, Puyo.make a;
                   0, 1, Puyo.make a;
                   1, 0, Puyo.make b;
                   1, 1, Puyo.make b ]
         else
           Quad (a, next_colors next_color4 a))
      combs
  in
  twos @ threes1 @ threes2 @ fours

let random colors =
  let blocks = Array.of_list (all_blocks colors) in
  let count = Array.length blocks in
  let gen state rand =
    let rand, n = Rand.int rand count in
    rand, state, blocks.(n)
  in {
    generator = gen;
    state = 0;
  }

let next gen rand = gen.generator gen rand
