(** Client / server connections *)

type 'a connection
  (** The type of connections. *)

type 'a server
  (** The type of servers. *)

val listen: ?addr: string list -> int -> 'a server
  (** Make a new server.

      [listen port]: make a server listening on [port] on addresses [addr].
      If [addr] is omitted, listen to all addresses. *)

val accept: ?max: int -> 'a server -> 'a connection list
  (** Accept pending connections.

      [accept serv]: accept incoming connections on server [serv]. At most
      [max] connections are returned (default is infinite). *)

val connect: string -> int -> 'a connection
  (** Connect to a server.

      [connect addr port]: connect to server at address [addr]
      on port [port]. *)

val close: 'a connection -> unit
  (** Close a connection.

      Packets that have not been received yet are lost. *)

val stop: 'a server -> unit
  (** Stop a server. *)

val ready: 'a connection -> bool
  (** Test if a connection is ready.

      Connections returned by [accept] are already ready. Connections returned
      by [connect] are not ready until the server accepts the connection. *)

val active: 'a connection -> bool
  (** Test if a connection has been closed.

      Return [false] if one of the peers have closed the connection. *)

val send: 'a connection -> 'a -> unit
  (** Send data over a connection. *)

val receive: 'a connection -> 'a list
  (** Receive data over a connection. *)

val receive_filter: ('a -> bool) -> 'a connection -> 'a list
  (** Receive data matching a predicate.

      [receive p c]: same as [receive c] but only receive data matching
      predicate [p]. Other messages are not discarded, they can still
      be received. *)

val remote_address: 'a connection -> string
  (** Get the remote address of a connection. *)

val remote_port: 'a connection -> int
  (** Get the remote port of a connection. *)
