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

open Common

let addr = if Array.length Sys.argv > 1 then Sys.argv.(1) else "localhost"

let stop = ref false
let toto = ref 1
let tata = ref 1

let handle_msg cx = function
  | Toto i ->
      if i = !toto then begin
        if !stop then
          echo "Toto %d (but we stopped)" i
        else begin
          echo "Toto %d" i;
          Net.send cx (Toto i);
          incr toto
        end
      end else begin
        echo "Bad Toto: received %d, expected %d" i !toto;
        stop := true
      end
  | Tata i ->
      if i = !tata then begin
        if !stop then
          echo "Tata %d (but we stopped)" i
        else begin
          echo "Tata %d" i;
          Net.send cx (Tata i);
          incr tata
        end
      end else begin
        echo "Bad Tata: received %d, expected %d" i !tata;
        stop := true
      end

let () =
  let cx = Net.connect addr 4269 in
  Net.send cx (Toto 0);
(*  Net.send cx (Tata 0);*)
  while true do
    Udp.wait_for_input ~timeout: 1. ();
    List.iter (handle_msg cx) (Net.receive cx)
  done
