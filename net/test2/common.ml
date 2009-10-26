module Protocol = struct
  type message =
    | Toto of int
    | Tata of int

  let channel = function
    | Toto _
    | Tata _ -> 0

  let channels = [ 0, Net.Ordered ]

  let encode buf = function
    | Toto i ->
        Bin.write buf Bin.int 0;
        Bin.write buf Bin.int i
    | Tata i ->
        Bin.write buf Bin.int 1;
        Bin.write buf Bin.int i

  let decode buf =
    match Bin.read buf Bin.int with
      | 0 -> Toto (Bin.read buf Bin.int)
      | 1 -> Tata (Bin.read buf Bin.int)
      | _ -> failwith "test protocol decoder"

  let codec =
    Bin.custom encode decode
end

open Printf
open Unix

let echo x =
  ksprintf
    (fun s ->
       let time = localtime (time ()) in
       printf "[%02d:%02d:%02d] %s\n%!"
         time.tm_hour time.tm_min time.tm_sec s)
    x

module Net = Net.Make(Protocol)(Protocol)

include Protocol
