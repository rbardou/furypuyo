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

(** Fury Puyo network protocol *)

module ToServer = struct
  type message =
    | MyName of string
    | MyPassword of string
    | MyScore of Score.t
    | GetScores of int (* position (only give position .. position + 9) *)
    | GetRoomList
    | NewRoom
    | JoinRoom of int
    | LeaveRoom
    | Ready
    | SendGarbage of int
    | FinishGarbage

  let channel = function
    | MyName _
    | MyPassword _
    | SendGarbage _
    | FinishGarbage ->
        0
    | MyScore _
    | GetScores _
    | GetRoomList
    | NewRoom
    | JoinRoom _
    | LeaveRoom
    | Ready ->
        1

  let channels =
    [ 0, Net.Ordered;
      1, Net.Important ]

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
      | MyScore s ->
          wi 2;
          w Score.codec s
      | GetScores i ->
          wi 3;
          wi i
      | GetRoomList ->
	  wi 4;
      | NewRoom ->
	  wi 5
      | JoinRoom i ->
	  wi 6;
	  wi i
      | LeaveRoom ->
	  wi 7
      | Ready ->
	  wi 8
      | SendGarbage i ->
          wi 9;
          wi i
      | FinishGarbage ->
          wi 10

  let decode buf =
    let r x = Bin.read buf x in
    let ri () = r Bin.int in
    let rs () = r Bin.string in
    match ri () with
      | 0 -> MyName (rs ())
      | 1 -> MyPassword (rs ())
      | 2 -> MyScore (r Score.codec)
      | 3 -> GetScores (ri ())
      | 4 -> GetRoomList
      | 5 -> NewRoom
      | 6 -> JoinRoom (ri ())
      | 7 -> LeaveRoom
      | 8 -> Ready
      | 9 -> SendGarbage (ri ())
      | 10 -> FinishGarbage
      | _ -> failwith "Protocol.ToServer.decode"

  let codec =
    Bin.custom encode decode
end

module ToClient = struct
  type message =
    | YourNameExists of bool
    | YouAreConnected
    | WrongPassword
    | Score of int * string * Score.t (* position, player, score *)
    | RoomList of (string * int) list
    | JoinedRoom of string * int (* room's name, room's identifier *)
    | RoomPlayers of (string * bool) list (* player's name, ready *)
    | StartGame
    | PrepareGarbage of int
    | ReadyGarbage of int

  let channel = function
    | YourNameExists _
    | YouAreConnected
    | WrongPassword
    | RoomList _
    | JoinedRoom _
    | StartGame
    | PrepareGarbage _
    | ReadyGarbage _ ->
        0
    | Score _
    | RoomPlayers _ ->
        1

  let channels =
    [ 0, Net.Ordered;
      1, Net.Important ]

  let encode buf m =
    let w x = Bin.write buf x in
    let wi = w Bin.int in
    let wb = w Bin.bool in
    let ws = w Bin.string in
    match m with
      | YourNameExists b ->
          wi 0;
          wb b
      | YouAreConnected ->
          wi 1
      | WrongPassword ->
          wi 2
      | Score (i, s, sc) ->
          wi 3;
          wi i;
          ws s;
          w Score.codec sc
      | RoomList l ->
	  wi 4;
	  w (Bin.list (Bin.couple Bin.string Bin.int)) l
      | JoinedRoom (s, i) ->
	  wi 5;
	  ws s;
	  wi i
      | RoomPlayers l ->
	  wi 6;
	  w (Bin.list (Bin.couple Bin.string Bin.bool)) l
      | StartGame ->
	  wi 7
      | PrepareGarbage i ->
          wi 8;
          wi i
      | ReadyGarbage i ->
          wi 9;
          wi i

  let decode buf =
    let r x = Bin.read buf x in
    let ri () = r Bin.int in
    let rb () = r Bin.bool in
    let rs () = r Bin.string in
    match ri () with
      | 0 -> YourNameExists (rb ())
      | 1 -> YouAreConnected
      | 2 -> WrongPassword
      | 3 ->
          let i = ri () in
          let s = rs () in
          let sc = r Score.codec in
          Score (i, s, sc)
      | 4 -> RoomList (r (Bin.list (Bin.couple Bin.string Bin.int)))
      | 5 ->
	  let s = rs () in
	  let i = ri () in
	  JoinedRoom (s, i)
      | 6 -> RoomPlayers (r (Bin.list (Bin.couple Bin.string Bin.bool)))
      | 7 -> StartGame
      | 8 -> PrepareGarbage (ri ())
      | 9 -> ReadyGarbage (ri ())
      | _ -> failwith "Protocol.ToClient.decode"

  let codec =
    Bin.custom encode decode
end

module Net = Net.Make(ToServer)(ToClient)
