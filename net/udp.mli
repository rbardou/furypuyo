(** UDP sockets *)

(** Abstraction of UNIX UDP sockets. Data are automatically marshaled.
    Addresses may be IP or DNS addresses. *)

exception Network_error of string * string
  (** Parameters are function name and error explaination. *)

type 'a socket
  (** The type of UDP sockets. *)

type addr
  (** The type of internet addresses. *)

val addr: string -> int -> addr
  (** Make an internet address.

      [addr a p] is the internet address [a], which may be an IP, IPv6 or DNS
      address, on port [p]. *)

val socket: unit -> 'a socket
  (** Make a new UDP socket. *)

val bind: 'a socket -> ?addr: string -> int -> unit
  (** Bind an UDP socket to a local address and port.

      [bind_local socket port]: bind [socket] to [port].
      Once a socket is bound to a port, it can receive data sent to this port
      on address [addr]. If [addr] is not specified, the socket is bound to
      all addresses (LAN, Internet, ...).

      If you do not [bind] a socket, it is bound to a random port chosen by
      the system. *)

val send: 'a socket -> addr -> 'a -> unit
  (** Send an UDP packet. *)

val receive: 'a socket -> (addr * 'a) list
  (** Receive UDP packets. *)

val close: 'a socket -> unit
  (** Close an UDP socket.

      The socket is no longer usable after this. *)
