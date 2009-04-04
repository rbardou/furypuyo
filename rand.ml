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

(* We cannot use the Random module, because it does not allow to save and
   reload the state of the PRNG, and is not persistent. So this is just an
   implementation of the UNIX PRNG. Anyway, for this game we do not really
   need a very good PNRG. *)

(* X(n+1) = (a * X(n) + c) mod m, where m = 2^32 *)

let a = 1103515245l
let c = 12345l

type t = Int32.t

let self_init () =
  Random.self_init ();
  Int32.of_int (Random.bits ())

let next r = Int32.add (Int32.mul a r) c

let int r bound =
  let rand =
    Int32.to_int
      (Int32.rem
         (Int32.abs (if bound >= 65536 then r else Int32.shift_right r 16))
         (Int32.of_int bound)) in
  next r, rand

let bool r =
  let b = Int32.compare (Int32.logand r 1048576l) 0l > 0 in (* 21th bit *)
  next r, b

let codec = Bin.int32
