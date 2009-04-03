type t =
  | V1 of int

let compare a b =
  match a, b with
    | V1 a, V1 b -> compare a b

let renew = function
  | V1 score -> V1 score

let encode buf scores =
  match scores with
    | V1 scores ->
        Bin.write buf Bin.int 1;
        Bin.write buf Bin.int scores

let decode buf =
  let scores =
    match Bin.read buf Bin.int with
      | 1 -> V1 (Bin.read buf Bin.int)
      | n ->
          raise (Highscores.Cannot_read_scores
                   ("unknown version: "^string_of_int n))
  in
  renew scores

let codec =
  Bin.custom encode decode

let make score =
  V1 score

let score = function
  | V1 score -> score
