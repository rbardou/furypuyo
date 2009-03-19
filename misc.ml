(** Miscellaneous stuff *)

let rec list_mapi acc i f = function
  | [] -> List.rev acc
  | x :: rem -> list_mapi (f i x :: acc) (i + 1) f rem
let list_mapi f l = list_mapi [] 0 f l

let rec list_iteri i f = function
  | [] -> ()
  | x :: rem ->
      f i x;
      list_iteri (i + 1) f rem
let list_iteri f l = list_iteri 0 f l

let rec split_when f ?(acc = []) = function
  | [] -> raise Not_found
  | x :: r ->
      if f x then List.rev acc, x, r else
        let acc = x :: acc in
        split_when f ~acc r

let rec list_last = function
  | [] -> raise (Invalid_argument "list_last")
  | [ x ] -> x
  | _ :: rem -> list_last rem

let rec list_trunc acc l = function
  | 0 -> List.rev acc
  | n ->
      match l with
        | [] -> List.rev acc
        | x :: rem ->
            list_trunc (x :: acc) rem (n - 1)
let list_trunc x = list_trunc [] x
