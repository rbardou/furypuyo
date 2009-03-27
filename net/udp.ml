open Unix

exception Network_error of string * string

type 'a socket = file_descr

type addr = sockaddr

let addr addr port =
  let addr =
    try
      (gethostbyname addr).h_addr_list.(0)
    with Not_found ->
      raise (Network_error ("Udp.addr", "Host not found: "^addr))
  in
  ADDR_INET (addr, port)

let make_addr = addr

let socket () =
  let sock = Unix.socket PF_INET SOCK_DGRAM 0 in
  Unix.set_nonblock sock;
  sock

let bind sock ?addr port =
  let addr =
    match addr with
      | None -> ADDR_INET (inet_addr_any, port)
      | Some addr -> make_addr addr port
  in
  Unix.bind sock addr

let close sock =
  Unix.close sock

let maximum_packet_size = 512

let send sock addr msg =
  let buf = Marshal.to_string msg [] in
  let len = String.length buf in
  assert (len <= maximum_packet_size);
  let sent = Unix.sendto sock buf 0 len [] addr in
  assert (sent = len)

let receive_one (sock: 'a socket) =
  let len = maximum_packet_size in
  let buf = String.create len in
  try
    let real_len, addr = Unix.recvfrom sock buf 0 len [] in
    assert (real_len > 0);
    Some (addr, (Marshal.from_string buf 0: 'a))
  with
    | Unix_error ((EAGAIN | EWOULDBLOCK), _, _) ->
        None
    | Unix_error (ECONNREFUSED, _, _) ->
        raise (Network_error ("Udp.receive", "Connection refused"))

let rec receive acc sock =
  match receive_one sock with
    | None -> List.rev acc
    | Some x -> receive (x :: acc) sock

let receive x = receive [] x

let string_of_addr = function
  | ADDR_UNIX _ ->
      assert false
  | ADDR_INET (addr, _) ->
      string_of_inet_addr addr

let port_of_addr = function
  | ADDR_UNIX _ ->
      assert false
  | ADDR_INET (_, port) ->
      port
