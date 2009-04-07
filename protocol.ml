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

module ToServer = struct
  type message =
    | MyName of string
    | MyPassword of string

  let channel = function
    | MyName _
    | MyPassword _ ->
        0

  let channels =
    [ 0, Net.Ordered ]

  let encode buf m =
    let w x = Bin.write buf x in
    let wi = w Bin.int in
    let ws = w Bin.string in
    match m with
      | MyName s ->
          wi 0;
          ws s
      | MyPassword s ->
          wi 1;
          ws s

  let decode buf =
    let r x = Bin.read buf x in
    let ri () = r Bin.int in
    let rs () = r Bin.string in
    match ri () with
      | 0 -> MyName (rs ())
      | 1 -> MyPassword (rs ())
      | _ -> failwith "Protocol.ToServer.decode"

  let codec =
    Bin.custom encode decode
end

module ToClient = struct
  type message =
    | YourNameExists of bool
    | YouAreConnected
    | WrongPassword

  let channel = function
    | YourNameExists _
    | YouAreConnected
    | WrongPassword ->
        0

  let channels =
    [ 0, Net.Ordered ]

  let encode buf m =
    let w x = Bin.write buf x in
    let wi = w Bin.int in
    let wb = w Bin.bool in
    match m with
      | YourNameExists b ->
          wi 0;
          wb b
      | YouAreConnected ->
          wi 1
      | WrongPassword ->
          wi 2

  let decode buf =
    let r x = Bin.read buf x in
    let ri () = r Bin.int in
    let rb () = r Bin.bool in
    match ri () with
      | 0 -> YourNameExists (rb ())
      | 1 -> YouAreConnected
      | 2 -> WrongPassword
      | _ -> failwith "Protocol.ToClient.decode"

  let codec =
    Bin.custom encode decode
end

module Net = Net.Make(ToServer)(ToClient)
