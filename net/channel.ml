open Connect

type 'a m = int * 'a

type 'a channel = 'a m connection * int

let channel c n = c, n

let send (c, n) m = send c (n, m)

let receive (c, n) = List.map snd (receive_filter (fun (n', _) -> n = n') c)
