open Misc

type 'a orderer = {
  frame: 'a Frame.frame;
  mutable next_deliverable: int;
  mutable buffer_before: (int * 'a) list;
  mutable buffer: 'a option F.t;
}

let start ?size frame =
  let size =
    match size with
      | None ->
          Frame.size frame
      | Some size ->
          if size <= 0 then 0 else size
  in
  {
    frame = frame;
    next_deliverable = 0;
    buffer_before = [];
    buffer = F.make size None None;
  }

let rec pop_some acc id buf =
  match F.get buf id with
    | None ->
        acc
    | Some msg ->
        F.shift buf (id + 1);
        pop_some ((id, msg) :: acc) (id + 1) buf

let pop_some buf = pop_some [] (F.position buf) buf

let rec shift_some acc buf n =
  if n <= 0 then
    acc
  else
    let acc =
      let id = F.position buf in
      match F.get buf id with
        | None -> acc
        | Some msg -> (id, msg) :: acc
    in
    shift_some acc buf (n - 1)

let shift_some buf n = shift_some [] buf n

let message ord (id, msg) =
  if id >= ord.next_deliverable then begin
    match F.state ord.buffer id with
      | Young ->
          let shift = id - F.position ord.buffer - F.size ord.buffer + 1 in
          ord.buffer_before <- ord.buffer_before @ shift_some ord.buffer shift;
          F.set ord.buffer id (Some msg);
          ord.buffer_before <- ord.buffer_before @ pop_some ord.buffer
      | InFrame ->
          F.set ord.buffer id (Some msg);
          ord.buffer_before <- ord.buffer_before @ pop_some ord.buffer
      | Old ->
          ord.buffer_before <- (id, msg) :: ord.buffer_before
  end

let update ord =
  List.iter (message ord) (Frame.receive ord.frame)

let receive ord =
  update ord;
  let b = ord.buffer_before in
  ord.buffer_before <- [];
  List.map snd (List.sort (fun (a, _) (b, _) -> compare a b) b)
