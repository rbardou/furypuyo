open Puyo

let width = 6
let height = 14

type game = {
  field: Cell.t Matrix.t;
  incoming: Block.t;
  rand: Rand.t;
  generator: Generator.t;
}

let start () =
  let generator = Generator.random [ Red; Green; Blue; Yellow ] in
  let rand = Rand.self_init () in
  let rand, generator, incoming = Generator.next generator rand in
  {
    field = Matrix.make width height Cell.empty;
    incoming = incoming;
    rand = rand;
    generator = generator;
  }
