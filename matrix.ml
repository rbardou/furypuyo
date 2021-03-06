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

module PosMap = Map.Make(struct type t = int * int let compare = compare end)

type 'a data = 'a PosMap.t

type 'a t = {
  width: int;
  height: int;
  default: 'a;
  data: 'a data;
}

let make w h v = {
  width = w;
  height = h;
  default = v;
  data = PosMap.empty;
}

let get f x y = try PosMap.find (x, y) f.data with Not_found -> f.default

let set f x y v = { f with data = PosMap.add (x, y) v f.data }

let width f = f.width

let height f = f.height

let inside f x y = x >= 0 && y >= 0 && x < f.width && y < f.height

let codec_data a =
  Bin.convert
    (fun m -> PosMap.fold (fun a b acc -> (a, b) :: acc) m [])
    (List.fold_left (fun acc (a, b) -> PosMap.add a b acc) PosMap.empty)
    (Bin.list (Bin.couple (Bin.couple Bin.int Bin.int) a))

let encode a buf m =
  Bin.write buf Bin.int m.width;
  Bin.write buf Bin.int m.height;
  Bin.write buf a m.default;
  Bin.write buf (codec_data a) m.data

let decode a buf =
  let width = Bin.read buf Bin.int in
  let height = Bin.read buf Bin.int in
  let default = Bin.read buf a in
  let data = Bin.read buf (codec_data a) in
  {
    width = width;
    height = height;
    default = default;
    data = data;
  }

let codec a =
  Bin.custom (encode a) (decode a)
