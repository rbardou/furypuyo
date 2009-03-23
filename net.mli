(** Simple network abstraction *)

exception Network_error of string * string
  (** General system or network error.

      The first parameter is the name of the function which raised the error.
      The second parameter is a description of the error. *)

exception Server_is_stopped
  (** Server has been stopped. *)

exception Connection_is_closed
  (** Connection has been closed by one of the peers. *)

(** Network protocol description *)
module type PROTOCOL = sig
  type message
    (** The type of protocol messages.

        No closure and no class should appear in the implementation of this
        type. *)

  val ordered: message -> bool
    (** Guarantee the order of messages.

        Ordered messages are guaranteed to be delivered in the same order
        they are sent. This introduces more latency. *)

  val important: message -> bool
    (** Guarantee message delivery.

        If a message is not important and does not reach its destination,
        nothing is done to resend it. If it does reach its destination but
        is ordered and a younger ordered message has already been received and
        treated, it is forgotten. *)
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

  val listen: int -> server
    (** Establish a server on a given port. *)

  val accept: server -> connection option
    (** Accept a connexion.

        If a client is trying to connect to the server, return a socket to
        its connexion. Else, return [None]. The connection is returned
        ready.

        May raise [Server_is_stopped]. *)

  val connect: string -> int -> connection
    (** Connect to a server.

        [connect address port]: connect to the server at [address], which
        may be an IP address or a DNS address, at the given [port].

        This is non-blocking. If you send data to an unconnected connection,
        it will be buffered and send when the connection is ready. You have
        to handle timeouts yourself. *)

  val ready: connection -> bool
    (** Test whether a connection is ready.

        If the result is [true], the connection is established.
        If connection is closed, return [false]. *)

  val send: connection -> message -> unit
    (** Send a message.

        May raise [Connection_is_closed]. *)

  val receive: connection -> message option
    (** Receive a message.

        If no message is ready to be received, return [None].

        May raise [Connection_is_closed], but only if no message is waiting
        to be received. *)

  val close: connection -> unit
    (** Close a connection.

        If the connection is already closed, do nothing. *)

  val active: connection -> bool
    (** Test whether a connection is active.

        Value is [false] if one of the peer has closed the connection. This
        is not the same as [ready]. *)

  val stop: server -> unit
    (** Stop listening for connections.

        If the server is already stopped, do nothing. *)
end

module Make(P: PROTOCOL): NET with type message = P.message
