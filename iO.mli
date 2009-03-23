(**************************************************************************)
(* Copyright (c) 2009, Romain BARDOU                                      *)
(* All rights reserved.                                                   *)
(*                                                                        *)
(* Redistribution and  use in  source and binary  forms, with  or without *)
(* modification, are permitted provided that the following conditions are *)
(* met:                                                                   *)
(*                                                                        *)
(* * Redistributions  of  source code  must  retain  the above  copyright *)
(*   notice, this list of conditions and the following disclaimer.        *)
(* * Redistributions in  binary form  must reproduce the  above copyright *)
(*   notice, this list of conditions  and the following disclaimer in the *)
(*   documentation and/or other materials provided with the distribution. *)
(* * Neither the name of Fury Puyo nor  the names of its contributors may *)
(*   be used  to endorse or  promote products derived  from this software *)
(*   without specific prior written permission.                           *)
(*                                                                        *)
(* THIS SOFTWARE  IS PROVIDED BY  THE COPYRIGHT HOLDERS  AND CONTRIBUTORS *)
(* "AS  IS" AND  ANY EXPRESS  OR IMPLIED  WARRANTIES, INCLUDING,  BUT NOT *)
(* LIMITED TO, THE IMPLIED  WARRANTIES OF MERCHANTABILITY AND FITNESS FOR *)
(* A PARTICULAR PURPOSE  ARE DISCLAIMED. IN NO EVENT  SHALL THE COPYRIGHT *)
(* OWNER OR CONTRIBUTORS BE  LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, *)
(* SPECIAL,  EXEMPLARY,  OR  CONSEQUENTIAL  DAMAGES (INCLUDING,  BUT  NOT *)
(* LIMITED TO, PROCUREMENT OF SUBSTITUTE  GOODS OR SERVICES; LOSS OF USE, *)
(* DATA, OR PROFITS; OR BUSINESS  INTERRUPTION) HOWEVER CAUSED AND ON ANY *)
(* THEORY OF  LIABILITY, WHETHER IN  CONTRACT, STRICT LIABILITY,  OR TORT *)
(* (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING  IN ANY WAY OUT OF THE USE *)
(* OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.   *)
(**************************************************************************)

(** Input / output abstraction *)

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

val frame_delay: int -> bool
  (** Wait a specified number of milliseconds before returning.

      If [delay] was last called 5ms ago, and now you call [delay 8], the
      game will actually sleep for 3ms. If you call [delay 3], the system
      will not sleep at all. This is useful to ensure a fixed frame rate.

      Return [true] is the delay was applied, [false] if the last frame was too
      long. *)

val timer_start: unit -> unit
  (** Start or restart the timer.

      Should be called at the beginning of the first frame.
      If you do not call [timer_start], [frame_delay] will not be precise
      for the first frame. Worst, if you let your process run for a while
      without calling [frame_delay] (for example if the process is paused)
      then the next time you call [frame_delay], a huge jump in time will
      happen. You do not want that. *)

val close: unit -> unit
  (** Close the window.

      Should always be called before you exit. *)

val on_quit: (unit -> bool) -> unit
  (** Call a function when the user tries to close the window.

      By default, [close ()] and [exit 0] are called. You can use [on_exit]
      to change this behavior. The function you give may never return
      (if you want to exit yourself) or return a boolean. If this boolean is
      [true], the default behavior (closing and exiting) is then executed.
      Else, nothing happens.

      Note that this only works if you have at least one reader made with
      [MakeReader] and that you call its [read] function regularly. *)

(** Image making and drawing. *)
module Sprite: sig
  type t
    (** The type of sprites. *)

  val load: ?align: align -> ?transparency: [`NONE | `BLACK | `ALPHA] ->
    string -> t
    (** Load a sprite from a file.

        Transparency:
        - [`NONE] for no transparency (fastest) (default)
        - [`BLACK] so that all black or fully transparent pixels are transparent
        - [`ALPHA] for images with alpha channels (slowest) *)

  val draw: t -> int -> int -> unit
    (** Draw a sprite. *)

  val screenshot: ?align: align -> unit -> t
    (** Get a screenshot.

        Return a sprite whose image is the content of the current screen.
        By "screen" we mean "screen buffer", i.e. not the actual content of the
        screen but what would be on the screen after an [update]. *)

  val width: t -> int
    (** Get the width of a sprite. *)

  val height: t -> int
    (** Get the height of a sprite. *)

  val align: t -> align -> t
    (** Change the alignment of a sprite.

        This is persistent and efficient. *)
end

(** Font loading, making and writing. *)
module Text: sig
  type t
    (** The type of fonts. *)

  val load: string -> int -> Sdlvideo.color -> t
    (** Load a TTF font from a file.

        [load file size color]: load a TTF font from file [file]
        with font size [size] and color [color]. *)

  val make: (char -> Sprite.t option * int) -> t
    (** Make a font from sprites.

        The argument [f] returns the sprite for a given chararacter.
        It may return [None] if no sprite exist for this character.
        Function [f] also returns an integer that will be added to the width
        of the character.

        You can make spaces by returning [None, size] where [size] is the size
        (in pixels) of the space. *)

  val write: t -> ?align: align -> int -> int ->
    string -> unit
    (** Write some text.

        [write font x y text]: write [text] at position [(x, y)] using font
        [font]. *)

  val size: t -> string -> int * int
    (** Get the width and height of a text in a given font. *)
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

  val reset: unit -> unit
    (** Reset all pressed keys.

        Call this when you switch from a reader to another, from a screen
        to another, and so on. *)
end
