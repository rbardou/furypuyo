(** Encoding and decoding data in a stream *)

type 'a t
  (** The type of encoder / decoders.

      An ['a t] can be used to encode or decode data of type ['a]. *)

(** {2 Encoding} *)

val to_buffer: 'a t -> Buffer.t -> 'a -> unit

val to_channel: 'a t -> out_channel -> 'a -> unit

val to_string: 'a t -> 'a -> string

val to_my_string: 'a t -> string -> ?pos: int -> 'a -> unit

(** {2 Decoding} *)

val of_buffer: 'a t -> Buffer.t -> 'a

val of_channel: 'a t -> in_channel -> 'a

val of_string: 'a t -> ?pos: int -> string -> 'a

(** {2 Constructors} *)

val char: char t

val string: string t

val int: int t

val couple: 'a t -> 'b t -> ('a, 'b) t

val triple: 'a t -> 'b t -> 'c t -> ('a, 'b, 'c) t

val quad: 'a t -> 'b t -> 'c t -> 'd t -> ('a, 'b, 'c, 'd) t

val list: ?min: int -> ?max: int -> 'a t -> 'a list t

