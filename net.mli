(** Simple network abstraction *)

exception Network_error of string * string
  (** General system or network error.

      The first parameter is the name of the function which raised the error.
      The second parameter is a description of the error. *)

exception Server_is_stopped
  (** Server has been stopped. *)

(** Network protocol description *)
module type PROTOCOL = sig
  type message
    (** The type of protocol messages.

        No closure and no class should appear in the implementation of this
        type. *)

  val important: message -> bool
    (** Guarantee message delivery.

        If a message is not important and does not reach its destination,
        nothing is done to resend it. If it does reach its destination but
        is ordered and a younger ordered message has already been received and
        treated, it is forgotten. *)

  val order_count: int
    (** Number of different orders between messages. *)

  val ordered: message -> int option
    (** Guarantee the order of messages.

        [ordered msg] shall return [None] if [msg] is not ordered, or
        [Some order] if [msg] is ordered in the [order]th order.
        Messages which are ordered in the same [order]
        are guaranteed to be delivered in the same order
        they are sent.
        [order] shall be between [0] and [order_count - 1].

        Warning: important ordered messages and non important ordered messages
        must not be ordered in the same order.

        Ordered messages introduce more latency. *)
end

(** Non-blocking network input / ouput *)
module type NET = sig
  (** All functions may raise [Network_error]. *)

  type message
    (** The type of protocol messages. *)

  type server
    (** The type of network servers. *)

  type connection
    (** The type of network connections. *)

  val listen: ?addr: string list -> int -> server
    (** Establish a server on a given port.

        Addresses can be IPv4 (ex: ["127.0.0.1"]) or DNS addresses
        (ex: ["localhost"]).

        @param addr list of local addresses to listen. In practice, this limits
        the interfaces clients may connect from. For example, if you only give
        localhost (["127.0.0.1"]), only programs from the same computer will
        be able to connect to the server. If you add the internet IP address
        of the computer, all clients from the internet will be able to connect
        to the server. If the list is empty (default value), the server listens
        to all addresses at the same time. *)

  val accept: ?max: int -> server -> connection list
    (** Accept a connexion.

        If some clients are trying to connect to the server, return sockets to
        their connexion. Else, return the empty list. Connections are returned
        ready.

        May raise [Server_is_stopped].

        @param max the maximum number of connections to be accepted
        (default: infinite) *)

  val connect: string -> int -> connection
    (** Connect to a server.

        [connect address port]: connect to the server at [address], which
        may be an IP address or a DNS address, at the given [port].

        Addresses can be IPv4 (ex: ["127.0.0.1"]) or DNS addresses
        (ex: ["localhost"]).

        This is non-blocking. If you send data to an unconnected connection,
        it will be buffered and send when the connection is ready. You have
        to handle timeouts yourself. *)

  val send: connection -> message -> unit
    (** Send a message.

        If connection is closed, do nothing. *)

  val receive: ?max: int -> connection -> message list
    (** Receive a message.

        If no message is ready to be received, or if the connection is not
        active, return [None].

        @param max the maximum number of messages to be received
        (default: infinite) *)

  val close: connection -> unit
    (** Close a connection.

        If the connection is already closed, do nothing. *)

  val active: connection -> bool
    (** Test whether a connection is active.

        Value is [false] if one of the peer has closed the connection. *)

  val ready: connection -> bool
    (** Test whether a connection is established.

        Value is [true] if the other end of the connection acknowledged our
        presence, even if the connection has been closed. *)

  val stop: server -> unit
    (** Stop listening for connections.

        If the server is already stopped, do nothing. *)

  val remote_address: connection -> string
    (** Get the remote address of a connection. *)

  val remote_port: connection -> int
    (** Get the remote port of a connection. *)

  val connections: server -> connection list
    (** Get the active connections of a server. *)
end

module Make(P: PROTOCOL): NET with type message = P.message
