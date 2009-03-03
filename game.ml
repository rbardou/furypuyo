open Data

let width = 6
let height = 12

let empty_cell = {
  puyo = None;
}

let start () = {
  field = Field.make width height empty_cell;
  incoming = 
}

let rotate_left = List.map (fun (x, y, p) -> -y, x, p)

let rotate_right = List.map (fun (x, y, p) -> y, -x, p)
