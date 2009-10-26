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
