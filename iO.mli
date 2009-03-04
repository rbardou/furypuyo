(** Input / output abstraction. *)

type align =
  | Center
  | Left
  | Right
  | Top
  | Bottom
  | TopLeft
  | TopRight
  | BottomLeft
  | BottomRight
  | Custom of float * int * float * int
      (** Custom position. *)
  (** Position of the hot spot of an image.

      When the image is drawn at position [(x, y)], the image is moved so that
      the hot spot is placed at [(x, y)].

      Default value is [TopLeft].

      A custom position of [(xf, xp, yf, yp)] will place the hot spot
      at [(xf * w + xp, yf * h + yp)], where [w] and [h] are the width and
      height of the image. The position is relative to the top left of the
      image (the default hot spot). For instance, [Bottom] is equivalent to
      [Custom(0.5, 0, 1., 0)]. *)

val init: int -> int -> unit
  (** Initialize the module and open a window.

      [init w h]: open a window of width [w] and of height [h]. *)

val update: unit -> unit
  (** Update the window.

      This must be called after you finish drawing a frame. If you don't,
      nothing will change on the screen. *)

val frame_delay: int -> unit
  (** Wait a specified number of milliseconds before returning.

      If [delay] was last called 5ms ago, and now you call [delay 8], the
      game will actually sleep for 3ms. If you call [delay 3], the system
      will not sleep at all. This is useful to ensure a fixed frame rate. *)

val quit: unit -> unit
  (** Close the window.

      Should always be called before you exit. *)

(** TTF font loading and writing. *)
module Text: sig
  type t
    (** The type of TTF fonts. *)

  val load: string -> int -> t
    (** Load a TTF font from a file.

        [load file size]: load a TTF font from file [file]
        with font size [size]. *)

  val write: t -> ?align: align -> ?color: Sdlvideo.color -> int -> int ->
    string -> unit
    (** Write some text.

        [write font x y text]: write [text] at position [(x, y)] using font
        [font]. *)
end

(** Image making and drawing. *)
module Sprite: sig
  type t
    (** The type of sprites. *)

  val load: ?align: align -> string -> t
    (** Load a sprite from a file. *)

  val draw: t -> int -> int -> unit
    (** Draw a sprite. *)

  val screenshot: ?align: align -> unit -> t
    (** Get a screenshot.

        Return a sprite whose image is the content of the current screen.
        By "screen" we mean "screen buffer", i.e. not the actual content of the
        screen but what would be on the screen after an [update]. *)
end

(** Action descriptor. *)
module type ACTION = sig
  type t
    (** The type of actions. *)
end

(** Input registering and reading. *)
module MakeReader(A: ACTION): sig
  val read: unit -> A.t list
    (** Read inputs and return corresponding actions. *)

  val key_continuous: Sdlkey.t -> A.t -> unit
    (** Register a continuous key action.

        The action will be returned by [read] if the key is down
        (i.e. has been pressed and not released yet) at the moment of
        reading. *)

  val key_down: Sdlkey.t -> A.t -> unit
    (** Register a key down action.

        The action will be returned by [read] if the key has been pressed
        since the last call to [read]. *)

  val key_up: Sdlkey.t -> A.t -> unit
    (** Register a key up action.

        The action will be returned by [read] if the key has been released
        since the last call to [read]. *)

  val key_auto: int -> int -> Sdlkey.t -> A.t -> unit
    (** Register a repeatable key action.

        [key_auto ini rep k a]: the action [a] will be returned when [k] is
        pressed (i.e. goes from up to down). If the key stays pressed for
        [ini] initial milliseconds, the action is repeated. Then the action
        will be repeated every [rep] milliseconds as long as [k] stays
        pressed. *)
end
