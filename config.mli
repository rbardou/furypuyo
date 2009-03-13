(** Configuration file reading and writing *)

val init: ?var: string -> string -> unit
  (** Initialize the configuration module.

[init dir]: [dir] is the
directory where to look for configuration files. It may start with
[~/] to be relative to the current user home directory. For example,
[~/.mozilla/firefox] would be the FireFox directory.

@param var name of an environment variable which may be used to
override [dir]. *)

val filename: string -> string
  (** Get a configuration file name.

[filename file] converts [file] into a configuration file name.
If [file] is implicit (see the documentation
of the [Filename] module of the standard library), it is appended to the
configuration directory as with [Filename.concat].
Otherwise, it is left as is.

Use this function to load files which sit in the configuration directory
but that are not really text configuration files. Otherwise, use [load]. *)

(** {2 Text Configuration Files} *)

(** A text configuration file is a list of variables of the form
[NAME=VALUE], with comments of the form [# some comment].

[NAME] must start with a letter and continue with letters, digits or
underscores. It is not case sensitive.

Spaces and tabulations before and after the [=], end at the end of [VALUE],
are ignored. Comments may be added after [VALUE].
[VALUE] can be put in double quotes ["..."]. *)

type text_file
  (** The type of configuration files. *)

type 'a var
  (** The type of configuration variables. *)

val get: 'a var -> 'a
  (** Get the current value of a configuration variable. *)

val set: 'a var -> 'a -> unit
  (** Set the value of a configuration variable.

The value is not saved into the file until you call [save]. *)

val load: string -> string -> text_file
  (** Open a configuration file.

[load file desc] loads file [file]. Argument [desc] can contain a short
description of the file that will be added as a comment is the file
is to be created.

If you give an implicit file name, such as ["toto/tata.cfg"], it will be
looked for in the configuration directory given to [init].

If the file does not exist, it is not created unless you write a new variable
to it and [save] it. *)

val save: text_file -> unit
  (** Save a configuration file.

The contents of variables are changed in-place, so that if the user
modified the file by hand, it will not look all messed up. New variables
are added at the end of the file. Old variables, even those who were not
modified or used, are left as they were.

If no variable were modified, the configuration file is not written. *)

val custom: (string -> 'a option) -> ('a -> string) -> text_file ->
  string -> string -> 'a -> 'a var
  (** Get a configuration variable.

[custom of_string to_string file var desc def] reads the value of [var] in
[file].
If [file] does not contain [var], default value [def] is used.
Function [of_string] is used to convert the string value from the file.
It may return [None] if the value cannot be read.
Function [to_string] is used to convert the value to a string.
[desc] is a short description of the variable. It will be written as a comment
in the configuration file if the variable is to be added. *)

(** The following functions are instances of [custom]. *)

val string: text_file -> string -> string -> string -> string var
  (** Get a string configuration variable. *)

val int: text_file -> string -> string -> int -> int var
  (** Get an integer configuration variable. *)

val float: text_file -> string -> string -> float -> float var
  (** Get a float configuration variable. *)

val bool: text_file -> string -> string -> bool -> bool var
  (** Get a bool configuration variable.

Values for [true] may be [YES], [TRUE] or [1] and are not case-sensitive.

Values for [false] may be [NO], [FALSE] or [0] and are not case-sensitive. *)
