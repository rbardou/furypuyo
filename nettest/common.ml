open Printf
open Unix

let echo x =
  ksprintf
    (fun s ->
       let time = localtime (time ()) in
       printf "[%02d:%02d:%02d] %s\n%!"
         time.tm_hour time.tm_min time.tm_sec s)
    x

module Net = Net.Make(Protocol)

include Protocol

let string_of_msg = function
  | Unit -> "Unit"
  | Int i -> sprintf "Int %d" i
  | Bool b -> sprintf "Bool %b" b
  | String s -> sprintf "String %S" s

let send con msg =
  echo "Sending %s to %s, port %d"
    (string_of_msg msg) (Net.remote_address con) (Net.remote_port con);
  Net.send con msg
