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
        Bin.write Bin.int buf 1;
        Bin.write Bin.int buf scores

let decode buf =
  let scores =
    match Bin.read Bin.int buf with
      | 1 -> V1 (Bin.read Bin.int buf)
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
