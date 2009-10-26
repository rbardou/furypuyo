open Common

let handle_msg cx = function
  | Toto i -> Net.send cx (Toto (i + 1))
  | Tata i -> Net.send cx (Tata (i + 1))

let handle_cx cx =
  List.iter (handle_msg cx) (Net.receive cx)

let () =
  let server = Net.listen 4269 in
  let cxs = ref [] in
  while true do
    Udp.wait_for_input ~timeout: 1. ();
    cxs := Net.accept server @ !cxs;
    List.iter handle_cx !cxs
  done
