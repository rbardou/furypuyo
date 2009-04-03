(** Encoding and decoding data in a stream *)

(** The only assumptions this module makes is that system is at least 8-bits,
    and that the [char] type is represented using 8-bits integers.

    In particular, it is compatible between:
    - 32-bits and 64-bits,
    - little endiand and big endian,
    - negative integers with or without two's complement. *)

exception End_of_string
  (** The end of the input or output string has been reached. *)

exception Bad_identifier of string
  (** The identifier given in argument is not present. *)

type 'a t
  (** The type of type descriptors.

      An ['a t] can be used to encode or decode data of type ['a]. *)

type input
  (** The type of binary stream inputs. *)

type output
  (** The type of binary stream outputs. *)

type ('a, 'b) either =
  | This of 'a
  | That of 'b
  (** Either this or that. *)

(** {2 Input / Output} *)

val from_channel: in_channel -> input
  (** Make an input that will read from a channel. *)

val from_string: ?pos: int -> string -> input
  (** Make an input that will read from a string.

      When reading from such an input, if the end of the string is reached,
      exception [End_of_string] is raised.

      If you read several values from the same string input, they will be
      read in a sequence (the position is not reset).

      @param pos the position of the first character to be read from the string.
      Default is [0]. *)

val from_custom: (unit -> char) -> input
  (** Make an custom input.

      The given function will be called when a character has to be read. *)

val to_channel: out_channel -> output
  (** Make an output that will write to a channel. *)

val to_string: ?pos: int -> string -> output
  (** Make an output that will write to a string.

      When writing to such an output, if the end of the string is reached,
      exception [End_of_string] is raised.

      If you write several values to the same string output, they will be
      written in a sequence (the position is not reset).

      @param pos the position of the first character to be written to the
      string. Default is [0]. *)

val to_buffer: Buffer.t -> output
  (** Make an output that will write to a buffer.

      If you write several values to the same buffer, they will be
      written in a sequence (the buffer is not reset). *)

val to_custom: (char -> unit) -> output
  (** Make an custom output.

      The given function will be called when a character has to be written. *)

(** {2 High-level Encoding and Decoding} *)

val write: output -> 'a t -> 'a -> unit
  (** Write a value to an output.

      [write out t v]: write value [v] on output [out] using type descriptor
      [t]. *)

val read: input -> 'a t -> 'a
  (** Read a value from an output.

      [read out t]: read a value from output [out] using type descriptor [t]. *)

(** {3 Constructors} *)

val char: char t

val string: string t

val int: int t

val int32: Int32.t t

val int64: Int64.t t

val bool: bool t

val couple: 'a t -> 'b t -> ('a * 'b) t

val list: 'a t -> 'a list t

val option: 'a t -> 'a option t

val either: 'a t -> 'b t -> (('a, 'b) either) t

val identifier: string -> unit t
  (** Check that some identifier is present.

      [identifier id]: write [id] when encoding. When decoding, check that
      [id] is present. The string [id] is copied, you may modify it later
      without disturbing the behavior.

      Exception [Bad_identifier id] is raised when decoding if the identifier
      is not present. *)

val convert: ('b -> 'a) -> ('a -> 'b) -> 'a t -> 'b t
  (** Make a custom type descriptor from conversion functions.

      [custom enc dec t]: encode value using [enc] to the type described by
      [t], and decode value using [dec].

      You should ensure that [dec (enc x) = x] if you want to read the same
      values that you write. *)

val custom: (output -> 'a -> unit) -> (input -> 'a) -> 'a t
  (** Make a custom type descriptor.

      [custom enc dec]: values will be written using [enc] and read using [dec].
      These functions should respectively call [write] and [read],
      in the same order, to write and read their values.

      It is less safe than [convert], as you must be careful with the order. *)
