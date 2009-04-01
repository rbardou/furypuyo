exception End_of_string

exception Bad_identifier of string

type input = {
  in_char: unit -> char;
}

type output = {
  out_char: char -> unit;
}

type ('a, 'b) either =
  | This of 'a
  | That of 'b
  (** The ``either this or that'' type. *)

let nothing () = ()

let from_channel ch =
  {
    in_char = (fun () -> input_char ch);
  }

let from_string ?(pos = 0) s =
  let position = ref 0 in
  let length = String.length s in
  let char () =
    if !position < length then
      let c = s.[!position] in
      incr position;
      c
    else
      raise End_of_string
  in
  {
    in_char = char;
  }

let from_custom char =
  {
    in_char = char;
  }

let fail s _ = failwith s

let to_channel ch =
  {
    out_char = output_char ch;
  }

let to_buffer buf =
  {
    out_char = Buffer.add_char buf;
  }

let to_string ?(pos = 0) s =
  let position = ref 0 in
  let length = String.length s in
  let char c =
    if !position < length then begin
      s.[!position] <- c;
      incr position
    end else
      raise End_of_string
  in
  {
    out_char = char;
  }

let to_custom char =
  {
    out_char = char;
  }

(******************************************************************************)

type 'a t = {
  enc: output -> 'a -> unit;
  dec: input -> 'a;
}

let write how buf v =
  how.enc buf v

let read how buf =
  how.dec buf

(******************************************************************************)

(* little endian representation of a positive integer *)
let rec repr_of_pint acc i =
  if i > 0 then
    let low = i land 255 in
    let high = i lsr 8 in
    repr_of_pint (low :: acc) high
  else
    acc
let repr_of_pint i = repr_of_pint [] i

(* result is modulo and thus may be negative if the represented integer is
   bigger than representable positive integers *)
let rec pint_of_repr acc = function
  | [] -> acc
  | x :: rem -> pint_of_repr (acc lsl 8 lor x) rem
let pint_of_repr r = pint_of_repr 0 r

let encode_int buf i =
  let pi = if i >= 0 then i else -i in
  let r = repr_of_pint pi in
  let len = List.length r in
  let h = if i >= 0 then len else len lor 128 in
  buf.out_char (Char.chr h);
  List.iter (fun i -> buf.out_char (Char.chr i)) (List.rev r)

let decode_int buf =
  let h = Char.code (buf.in_char ()) in
  let len = h land 127 in
  let neg = h land 128 > 0 in
  let rec read acc = function
    | 0 -> acc
    | n -> read (Char.code (buf.in_char ()) :: acc) (n - 1)
  in
  let r = read [] len in
  let i = pint_of_repr r in
  if neg then -i else i

(******************************************************************************)

let convert enc dec a =
  {
    enc = (fun buf x -> a.enc buf (enc x));
    dec = (fun buf -> dec (a.dec buf));
  }

let char =
  {
    enc = (fun buf -> buf.out_char);
    dec = (fun buf -> buf.in_char ());
  }

let couple a b =
  {
    enc = (fun buf (x, y) -> a.enc buf x; b.enc buf y);
    dec = (fun buf -> let x = a.dec buf in let y = b.dec buf in x, y);
  }

let int =
  {
    enc = encode_int;
    dec = decode_int;
  }

let bool =
  convert (function true -> '\001' | false -> '\000') ((<>) '\000') char

let string =
  let enc buf s =
    let len = String.length s in
    encode_int buf len;
    for i = 0 to len - 1 do
      buf.out_char s.[i]
    done
  in
  let dec buf =
    let len = decode_int buf in
    let s = String.create len in
    for i = 0 to len - 1 do
      s.[i] <- buf.in_char ()
    done;
    s
  in
  {
    enc = enc;
    dec = dec;
  }

let list a =
  let enc buf l =
    let len = List.length l in
    encode_int buf len;
    List.iter (a.enc buf) l
  in
  let dec buf =
    let len = decode_int buf in
    let rec read acc = function
      | 0 -> List.rev acc
      | n -> read (a.dec buf :: acc) (n - 1)
    in
    read [] len
  in
  {
    enc = enc;
    dec = dec;
  }

let option a =
  let enc buf = function
    | Some x ->
        bool.enc buf true;
        a.enc buf x
    | None ->
        bool.enc buf false
  in
  let dec buf =
    match bool.dec buf with
      | true ->
          Some (a.dec buf)
      | false ->
          None
  in
  {
    enc = enc;
    dec = dec;
  }

let either a b =
  let enc buf = function
    | This x ->
        bool.enc buf false;
        a.enc buf x
    | That y ->
        bool.enc buf true;
        b.enc buf y
  in
  let dec buf =
    match bool.dec buf with
      | false ->
          This (a.dec buf)
      | true ->
          That (b.dec buf)
  in
  {
    enc = enc;
    dec = dec;
  }

let custom enc dec =
  {
    enc = enc;
    dec = dec;
  }

let identifier id =
  let id = String.copy id in
  let len = String.length id in
  let enc buf () =
    for i = 0 to len - 1 do
      buf.out_char id.[i]
    done
  in
  let dec buf =
    try
      for i = 0 to len - 1 do
        let c = buf.in_char () in
        if id.[i] <> c then raise Exit
      done
    with Exit ->
      raise (Bad_identifier (String.copy id))
  in
  {
    enc = enc;
    dec = dec;
  }