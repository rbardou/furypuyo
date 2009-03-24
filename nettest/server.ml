open Common

let message msg =
  echo "Received: %s" (string_of_msg msg)

let main () =
  echo "Starting server on port 4269...";
  let serv = Net.listen 4269 in
  let cons = ref [] in
  echo "Listening...";
  while true do
    let new_cons = Net.accept serv in
    List.iter
      (fun con ->
         echo "New client: %s, port %d"
           (Net.remote_address con) (Net.remote_port con))
      new_cons;
    cons := !cons @ new_cons;
    let keep, remove = List.partition Net.active !cons in
    cons := keep;
    List.iter
      (fun con ->
         echo "Disconnected: %s, port %d"
           (Net.remote_address con) (Net.remote_port con))
      remove;
    List.iter (fun con -> List.iter message (Net.receive con)) !cons;
  done;
  Net.stop serv

let () =
  Unix.handle_unix_error main ()
