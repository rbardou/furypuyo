type t =
  | MyName of string
  | YourNameStatus of bool
  | MyPassword of string
  | YouAreConnected

let channel = function
  | MyName _
  | YourNameStatus _
  | MyPassword _
  | YouAreConnected ->
      0

let channels =
  [ 0, Net.Ordered ]

let encode buf m =
  let w x = Bin.write buf x in
  let wi = w Bin.int in
  let ws = w Bin.string in
  let wb = w Bin.bool in
  match m with
    | MyName s ->
        wi 0;
        ws s
    | YourNameStatus b ->
        wi 1;
        wb b
    | MyPassword s ->
        wi 2;
        ws s
    | YouAreConnected ->
        wi 3

let decode buf =
  let r x = Bin.read buf x in
  let ri () = r Bin.int in
  let rs () = r Bin.string in
  let rb () = r Bin.bool in
  match ri () with
    | 0 -> MyName (rs ())
    | 1 -> YourNameStatus (rb ())
    | 2 -> MyPassword (rs ())
    | 3 -> YouAreConnected
