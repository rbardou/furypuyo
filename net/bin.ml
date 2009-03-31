exception End_of_string

type input = {
  in_char: unit -> char;
  in_start: unit -> unit;
  in_finish: unit -> unit;
}

type output = {
  out_char: char -> unit;
  out_start: unit -> unit;
  out_finish: unit -> unit;
}

let nothing () = ()

let from_channel ch =
  {
    in_char = (fun () -> input_char ch);
    in_start = nothing;
    in_finish = nothing;
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
    in_start = nothing;
    in_finish = nothing;
  }

let from_custom ?(start = nothing) ?(finish = nothing) char =
  {
    in_char = char;
    in_start = start;
    in_finish = nothing;
  }

let fail s _ = failwith s

let from_file file =
  let fail = fail ("file input ("^file^")") in
  let ch = ref fail in
  {
    in_char = (fun () -> input_char (!ch ()));
    in_start = (fun () -> ch := let ch = open_in file in fun () -> ch);
    in_finish = (fun () -> close_in (!ch ()); ch := fail);
  }

let to_channel ch =
  {
    out_char = output_char ch;
    out_start = nothing;
    out_finish = nothing;
  }

let to_buffer buf =
  {
    out_char = Buffer.add_char buf;
    out_start = nothing;
    out_finish = nothing;
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
    out_start = nothing;
    out_finish = nothing;
  }

let to_custom ?(start = nothing) ?(finish = nothing) char =
  {
    out_char = char;
    out_start = start;
    out_finish = nothing;
  }

let to_file file =
  let fail = fail ("file output ("^file^")") in
  let ch = ref fail in
  {
    out_char = (fun c -> output_char (!ch ()) c);
    out_start = (fun () -> ch := let ch = open_out file in fun () -> ch);
    out_finish = (fun () -> close_out (!ch ()); ch := fail);
  }

(******************************************************************************)

type 'a t = {
  enc: output -> 'a -> unit;
  dec: input -> 'a;
}

let write how buf v =
  buf.out_start ();
  how.enc buf v;
  buf.out_finish ()

let read how buf =
  buf.in_start ();
  let v = how.dec buf in
  buf.in_finish ();
  v

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

let custom enc dec a =
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
  custom (function true -> '\001' | false -> '\000') ((<>) '\000') char

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
