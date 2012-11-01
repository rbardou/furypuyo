(**************************************************************************)
(* Copyright (c) 2009, Romain BARDOU                                      *)
(* All rights reserved.                                                   *)
(*                                                                        *)
(* Redistribution and  use in  source and binary  forms, with  or without *)
(* modification, are permitted provided that the following conditions are *)
(* met:                                                                   *)
(*                                                                        *)
(* * Redistributions  of  source code  must  retain  the above  copyright *)
(*   notice, this list of conditions and the following disclaimer.        *)
(* * Redistributions in  binary form  must reproduce the  above copyright *)
(*   notice, this list of conditions  and the following disclaimer in the *)
(*   documentation and/or other materials provided with the distribution. *)
(* * Neither the name of Fury Puyo nor  the names of its contributors may *)
(*   be used  to endorse or  promote products derived  from this software *)
(*   without specific prior written permission.                           *)
(*                                                                        *)
(* THIS SOFTWARE  IS PROVIDED BY  THE COPYRIGHT HOLDERS  AND CONTRIBUTORS *)
(* "AS  IS" AND  ANY EXPRESS  OR IMPLIED  WARRANTIES, INCLUDING,  BUT NOT *)
(* LIMITED TO, THE IMPLIED  WARRANTIES OF MERCHANTABILITY AND FITNESS FOR *)
(* A PARTICULAR PURPOSE  ARE DISCLAIMED. IN NO EVENT  SHALL THE COPYRIGHT *)
(* OWNER OR CONTRIBUTORS BE  LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, *)
(* SPECIAL,  EXEMPLARY,  OR  CONSEQUENTIAL  DAMAGES (INCLUDING,  BUT  NOT *)
(* LIMITED TO, PROCUREMENT OF SUBSTITUTE  GOODS OR SERVICES; LOSS OF USE, *)
(* DATA, OR PROFITS; OR BUSINESS  INTERRUPTION) HOWEVER CAUSED AND ON ANY *)
(* THEORY OF  LIABILITY, WHETHER IN  CONTRACT, STRICT LIABILITY,  OR TORT *)
(* (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING  IN ANY WAY OUT OF THE USE *)
(* OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.   *)
(**************************************************************************)

(** Miscellaneous stuff *)

open Printf
open Unix

module IntMap =
  Map.Make (struct type t = int let compare = Pervasives.compare end)

let (|>) x f = f x

(** Use [enumerate a b] to get the list of integers from [a] to [b] (included)
    in increasing order. *)
let rec enumerate ?(acc = []) a b =
  if a > b then acc else enumerate ~acc: (b :: acc) a (b - 1)

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

let array_find f a =
  let r = ref 0 in
  try
    Array.iteri (fun i x -> if f x then (r := i; raise Exit)) a;
    raise Not_found
  with Exit ->
    !r

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

let rec str_split_char ?(acc = []) ?(pos = 0) c s =
  let len = String.length s in
  if pos > len then
    List.rev acc
  else if pos = len then
    List.rev ("" :: acc)
  else
    let index =
      try
        String.index_from s pos c
      with Not_found ->
        len
    in
    let sub = String.sub s pos (index - pos) in
    str_split_char ~acc: (sub :: acc) ~pos: (index + 1) c s

let rec new_file_name i base ext =
  let name =
    if i = 0 then base ^ ext
    else Printf.sprintf "%s_%d%s" base i ext
  in
  if Sys.file_exists name then
    new_file_name (i + 1) base ext
  else
    name
let new_file_name = new_file_name 0

let log x =
  ksprintf
    (fun s ->
       let time = localtime (time ()) in
       printf "[%02d:%02d:%02d] %s\n%!"
         time.tm_hour time.tm_min time.tm_sec s)
    x

let rec insert_in_sorted_list acc compare x = function
  | [] ->
      List.rev (x :: acc)
  | (y :: rem) as l ->
      if compare x y < 0 then
        List.rev_append acc (x :: l)
      else
        insert_in_sorted_list (y :: acc) compare x rem
let insert_in_sorted_list x = insert_in_sorted_list [] x

let rec insert_in_sorted_list_nodup acc compare x = function
  | [] ->
      List.rev (x :: acc)
  | (y :: rem) as l ->
      if compare x y < 0 then
        List.rev_append acc (x :: l)
      else if compare x y = 0 then
        List.rev_append acc l
      else
        insert_in_sorted_list_nodup (y :: acc) compare x rem
let insert_in_sorted_list_nodup x = insert_in_sorted_list_nodup [] x

let opt_map f x =
  match x with
    | None -> None
    | Some x -> Some (f x)

let opt_iter f x =
  match x with
    | None -> ()
    | Some x -> f x

let opt_fold f acc x =
  match x with
    | None -> acc
    | Some x -> f acc x

module type OrderedType = sig
  type t
  val compare: t -> t -> int
end

module SortedList(O: OrderedType): sig
  type t
  val empty: t
  val add: ?dup: bool -> O.t -> t -> t
  val remove: O.t -> t -> t
  val nth: int -> t -> O.t
  val count: t -> int
  val contents: t -> O.t list
  val sub: int -> int -> t -> O.t list
    (** Extract some elements.

        [sub i j l]: extract all items from [i] to [j].
        If [i > j] or if [i >= count l] or if [j < 0], return [[]].
        If [i < 0], behave as if [i = 0].
        If [j >= count l], behave as if [j = count l - 1]. *)
end = struct
  type t =
    | Leaf
    | Node of int * int * O.t * t * t (* height, count, value, left, right *)

  let empty = Leaf

  let count = function
    | Leaf -> 0
    | Node (_, c, _, _, _) -> c

  let height = function
    | Leaf -> 0
    | Node (h, _, _, _, _) -> h

  let left = function
    | Leaf -> invalid_arg "left"
    | Node (_, _, _, l, _) -> l

  let right = function
    | Leaf -> invalid_arg "right"
    | Node (_, _, _, _, r) -> r

  let value = function
    | Leaf -> invalid_arg "value"
    | Node (_, _, v, _, _) -> v

  let node v x y =
    Node (max (height x) (height y) + 1, count x + count y + 1, v, x, y)

  let rotate_left = function
    | Node (_, _, v, x, Node (_, _, w, y, z)) ->
        node w (node v x y) z
    | _ -> assert false

  let rotate_right = function
    | Node (_, _, v, Node (_, _, w, x, y), z) ->
        node w x (node v y z)
    | _ -> assert false

  let balance n =
    let v = value n in
    let l = left n in
    let r = right n in
    let lh = height l in
    let rh = height r in
    match lh - rh with
      | 2 ->
          let ll = left l in
          begin match lh - height ll with
            | 1 -> rotate_right n
            | 2 -> rotate_right (node v (rotate_left l) r)
            | _ -> assert false
          end
      | 1 | 0 | -1 ->
          node v l r
      | -2 ->
          let rr = right r in
          begin match rh - height rr with
            | 1 -> rotate_left n
            | 2 -> rotate_left (node v l (rotate_right r))
            | _ -> assert false
          end
      | _ ->
          assert false

  let rec add ?(dup = true) x = function
    | Leaf ->
        node x Leaf Leaf
    | Node (_, _, y, l, r) as tree ->
        if O.compare x y > 0 then
          balance (node y l (add ~dup x r))
        else if O.compare x y < 0 || dup then
          balance (node y (add ~dup x l) r)
        else
          tree

  let rec remove x = function
    | Leaf ->
        Leaf
    | Node (_, _, v, Leaf, Leaf) when O.compare x v = 0 ->
        Leaf
    | Node (_, _, v, l, r) ->
        if O.compare x v > 0 then
          balance (node v l (remove x r))
        else if O.compare x v < 0 then
          balance (node v (remove x l) r)
        else
          if height l > height r then
            let lv = value l in
            balance (node lv (remove lv l) r)
          else
            let rv = value r in
            balance (node rv l (remove rv r))

  let rec nth n = function
    | Leaf ->
        invalid_arg "nth"
    | Node (_, _, v, l, r) ->
        let lc = count l in
        if n < lc then
          nth n l
        else if n > lc then
          nth (n - lc - 1) r
        else
          v

  (* "acc" serves not to make the function tail-rec but to avoid using "@" *)
  let rec contents acc = function
    | Leaf ->
        acc
    | Node (_, _, v, l, r) ->
        contents (v :: (contents acc r)) l

  let rec sub acc i j = function
    | Leaf ->
        acc
    | Node (_, c, v, l, r) ->
        let lc = count l in
        if j < lc then
          sub acc i j l
        else if i > lc then
          sub acc (i - lc - 1) (j - lc - 1) r
        else
          sub (v :: (sub acc 0 (j - lc - 1) r)) i (lc - 1) l

  let contents x = contents [] x

  let sub x = sub [] x
end

(*
module L = SortedList(struct type t = int let compare = compare end)
let l = L.empty;;
let l = L.add 10 l;;
let l = L.add 4 l;;
let l = L.add 5 l;;
let l = L.add 9 l;;
let l = L.add 0 l;;
let l = L.add 1 l;;
let l = L.add 1 l;;
let l = L.add 1 l;;
let l = L.add 8 l;;
let l = L.add 7 l;;
let l = L.add 8 l;;
let l = L.add 3 l;;
L.contents l;;
L.sub 3 8 l;;
L.sub 8 3 l;;
L.sub 3 3 l;;
L.sub 0 0 l;;
L.sub (-1) (-1) l;;
L.sub 100 100 l;;
L.sub 12 12 l;;
let l = L.remove 9 l;;
L.contents l;;
let l = L.remove 0 l;;
let l = L.remove 1 l;;
let l = L.remove 42 l;;
L.contents l;;
let l = L.add 5 l;;
let l = L.add 9 l;;
let l = L.add 0 l;;
let l = L.add 1 l;;
let l = L.add 1 l;;
let l = L.add 1 l;;
let l = L.add 8 l;;
L.contents l;;
*)
