open Udp
open Net
open Arg
let servaddr = ref "127.0.0.1"
let () = parse [] (fun x -> servaddr := x) "Usage: client [host]"

open Common

let main () =
  echo "Connecting to %s, port 4269..." !servaddr;
  let con = Net.connect !servaddr 4269 in
  echo "Waiting for server acknowledgement...";
  while not (Net.ready con) do
    ()
  done;
  echo "Ready.";
  send con Unit;
  send con (Int 42);
  send con (Bool true);
  send con (String "Hello, world!");
  send con (Int 69);
  send con (Int 666);
  echo "Receiving for 1s...";
  let now = Unix.gettimeofday () in
  while Unix.gettimeofday () < now +. 1. do
    List.iter message (Net.receive con)
  done;
  echo "Closing...";
  Net.close con;
  echo "Closed."

let () =
  try
    Unix.handle_unix_error main ()
  with Network_error (_, e) ->
    echo "Network error: %s" e