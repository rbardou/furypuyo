open Common

let main () =
  echo "Connecting to 127.0.0.1, port 4269...";
  let con = Net.connect "127.0.0.1" 4269 in
  echo "Waiting for server acknowledgement...";
  while not (Net.ready con) do
    ()
  done;
  echo "Ready.";
  send con Unit;
  send con (Int 42);
  send con (Bool true);
  send con (String "Hello, world!");
  echo "Receiving...";
  List.iter message (Net.receive con);
  echo "Closing...";
  Net.close con;
  echo "Closed."

let () =
  Unix.handle_unix_error main ()
