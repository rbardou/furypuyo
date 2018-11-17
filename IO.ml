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

open Sdlvideo
open Sdlevent
open Sdlttf

let real_screen = ref (fun () -> assert false)
let screen = ref (fun () -> assert false)

let width = ref 0
let height = ref 0

let real_width = ref 1280
let real_height = ref 1024

let last_tick = ref 0

let zoom = ref false

let () =
  Sdlttf.init ()

let init w h =
  if not !zoom then
    begin
      real_width := w;
      real_height := h;
    end;

  Sdl.putenv "SDL_VIDEO_CENTERED" "center";
  Sdl.init [`TIMER; `VIDEO];
  enable_events
    (make_mask [
       KEYDOWN_EVENT;
       KEYUP_EVENT;
       MOUSEBUTTONDOWN_EVENT;
       QUIT_EVENT;
     ]);
  width := w;
  height := h;
  let the_screen =
    set_video_mode
      ~w: !real_width
      ~h: !real_height
      ~bpp: 32
      [`HWSURFACE; `DOUBLEBUF]
  in
  real_screen := (fun () -> the_screen);

  if !zoom then
    begin
      let fake_screen =
        create_RGB_surface_format
          the_screen
          [ `HWSURFACE ]
          !real_width
          !real_height
      in
      screen := (fun () -> fake_screen);
    end
  else
    screen := (fun () -> the_screen);

  last_tick := Sdltimer.get_ticks ()

let fdp = ref 0
let fdt = ref 0
let fdc = ref 0
let fdb = ref 0

let frame_delay d =
  if !last_tick = 0 then begin
    Sdltimer.delay d;
    last_tick := Sdltimer.get_ticks ();
    true
  end else
    let now = Sdltimer.get_ticks () in
    let delay = d - now + !last_tick in
    incr fdc;
    fdt := !fdt + delay;
    fdp := !fdp + d;
    if delay > 0 then
      Sdltimer.delay delay
    else
      incr fdb;
    last_tick := !last_tick + d;
    delay >= 0

let timer_start () = last_tick := 0

let screen () = !screen ()

let update_zoom () =
  let zoom_x = float_of_int !real_width /. float_of_int !width in
  let zoom_y = float_of_int !real_height /. float_of_int !height in
  let zoomed =
    Sdlgfx.zoomSurface
      (screen ())
      zoom_x
      zoom_y
      false
  in
  blit_surface
    ~src: zoomed
    ~dst: (!real_screen ())
    ~dst_rect: {
      r_x = 0;
      r_y = 0;
      r_w = 0;
      r_h = 0;
    } ();
  flip (!real_screen ())

let update_no_zoom () =
  flip (!real_screen ())

let update () =
  if !zoom then update_zoom () else update_no_zoom ()

let close () =
  if !fdc > 0 then
    Printf.printf "CPU usage: %d%% (%d/%d frame overflows)\n%!"
      (100 * (!fdp - !fdt) / !fdp) !fdb !fdc;
  Sdl.quit ()

let on_quit = ref (fun () -> true)

let execute_on_quit () =
  if !on_quit () then begin
    close ();
    exit 0
  end

let on_quit f = on_quit := f

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

let hotxy w h = function
  | Center -> w / 2, h / 2
  | Left -> 0, h / 2
  | Right -> w, h / 2
  | Top -> w / 2, 0
  | Bottom -> w / 2, h
  | TopLeft -> 0, 0
  | TopRight -> w, 0
  | BottomLeft -> 0, h
  | BottomRight -> w, h
  | Custom (xf, xp, yf, yp) ->
      int_of_float (float_of_int w *. xf) + xp,
      int_of_float (float_of_int h *. yf) + yp

let print_format surf =
  let pfi = surface_format surf in
  let bpp = pfi.bits_pp in
  let rmask = pfi.rmask in
  let gmask = pfi.gmask in
  let bmask = pfi.bmask in
  let amask = pfi.amask in
  Printf.printf "%d, masks: %s, %s, %s, %s\n%!"
    bpp
    (Int32.to_string rmask)
    (Int32.to_string gmask)
    (Int32.to_string bmask)
    (Int32.to_string amask)

let make_opaque_surface w h =
  create_RGB_surface_format (screen ()) [`HWSURFACE] ~w ~h

let make_colorkey_surface w h =
  let pfi = surface_format (screen ()) in
  let bpp = pfi.bits_pp in
  let rmask = pfi.rmask in
  let gmask = pfi.gmask in
  let bmask = pfi.bmask in
  let amask = pfi.amask in
  let surf =
    create_RGB_surface
      [`HWSURFACE; `SRCCOLORKEY]
      ~w ~h
      ~bpp ~rmask ~gmask ~bmask ~amask
  in
  set_color_key surf 0l;
  surf

let make_surface transparency =
  match transparency with
    | `NONE -> make_opaque_surface
    | `BLACK -> make_colorkey_surface
    | `ALPHA -> assert false (* TODO *)

let copy_surface ?(x = 0) ?(y = 0) a b =
  blit_surface
    ~src: a
    ~dst: b
    ~dst_rect: {
      r_x = x;
      r_y = y;
      r_w = 0;
      r_h = 0;
    } ()

module Sprite = struct
  type t = {
    hotx: int;
    hoty: int;
    width: int;
    height: int;
    surface: surface;
  }

  let of_surface ?(align = TopLeft) surface =
    let w, h, _ = surface_dims surface in
    let hotx, hoty = hotxy w h align in
    {
      hotx = hotx;
      hoty = hoty;
      surface = surface;
      width = w;
      height = h;
    }

  let load ?align ?(transparency = `NONE) file =
    let surf = Sdlloader.load_image file in
    let w, h, _ = surface_dims surf in
    let surf_best =
      if transparency = `ALPHA then surf else (* TODO *)
        make_surface transparency w h
    in
    copy_surface surf surf_best;
    of_surface ?align surf_best

  let draw sprite x y =
    blit_surface
      ~src: sprite.surface
      ~dst: (screen ())
      ~dst_rect: {
        r_x = x - sprite.hotx;
        r_y = y - sprite.hoty;
        r_w = 0;
        r_h = 0;
      } ()

  let screenshot ?align () =
    let surface = make_opaque_surface !width !height in
    copy_surface (screen ()) surface;
    of_surface ?align surface

  let width x = x.width

  let height x = x.height

  let align x a =
    let hx, hy = hotxy x.width x.height a in
    { x with hotx = hx; hoty = hy }
end

module Text = struct
  type t =
    | TTF of Sdlttf.font * Sdlvideo.color
    | Sprites of (Sprite.t option * int) array

  let load file size color =
    TTF (open_font file size, color)

  let write_ttf font color ?(align = TopLeft) x y txt =
    let (w, h) = Sdlttf.size_text font txt in
    let txt = Sdlttf.render_text_solid font txt ~fg: color in
    let hotx, hoty = hotxy w h align in
    blit_surface
      ~src: txt
      ~dst: (screen ())
      ~dst_rect: {
        r_x = x - hotx;
        r_y = y - hoty;
        r_w = 0;
        r_h = 0;
      } ()

  let size_sprites font txt =
    let y1 = ref 0 in
    let y2 = ref 0 in
    let w = ref 0 in
    for i = 0 to String.length txt - 1 do
      match font.(Char.code txt.[i]) with
        | None, aw ->
            w := !w + aw
        | Some sprite, aw ->
            w := !w + sprite.Sprite.width + aw;
            y1 := min !y1 (- sprite.Sprite.hoty);
            y2 := max !y2 (sprite.Sprite.height - sprite.Sprite.hoty)
    done;
    !w, (!y2 - !y1)

  let size = function
    | TTF (font, _) -> Sdlttf.size_text font
    | Sprites font -> size_sprites font

  let write_sprites font ?(align = TopLeft) x y txt =
    let w, h = size_sprites font txt in
    let hotx, hoty = hotxy w h align in
    let x = ref (x - hotx) in
    let y = y - hoty in
    for i = 0 to String.length txt - 1 do
      match font.(Char.code txt.[i]) with
        | None, aw ->
            x := !x + aw
        | Some sprite, aw ->
            Sprite.draw sprite !x y;
            x := !x + sprite.Sprite.width + aw
    done

  let write = function
    | TTF (font, color) -> write_ttf font color
    | Sprites font -> write_sprites font

  let make font =
    let a = Array.init 256 (fun i -> font (Char.chr i)) in
    Sprites a
end

module type ACTION = sig
  type t
end

module Key = struct
  type t = Sdlkey.t
  let compare = compare
end
module KeyMap = Map.Make(Key)

module MakeReader(A: ACTION) = struct
  type one_more = Zero | One of int | More of int

  let continuous = ref KeyMap.empty
  let up = ref KeyMap.empty
  let down = ref KeyMap.empty
  let auto = ref KeyMap.empty
  let pressed_keys = ref KeyMap.empty

  let action mapref key acc : A.t list =
    try KeyMap.find key !mapref :: acc
    with Not_found -> acc

  let action_auto now key since acc =
    try
      let a, ini, rep = KeyMap.find key !auto in
      let next = match since with
        | Zero -> now
        | One since -> since + ini
        | More since -> since + rep
      in
      if next <= now then begin
        pressed_keys :=
          KeyMap.add key
            (if since = Zero then One next else More next)
            !pressed_keys;
        a :: acc
      end else acc
    with Not_found -> acc

  let read () =
    let now = Sdltimer.get_ticks () in
    let instant = ref [] in
    let rec read_events acc =
      match poll () with
        | None -> acc
        | Some event ->
            let acc = begin match event with
              | KEYDOWN ke ->
                  pressed_keys := KeyMap.add ke.keysym Zero !pressed_keys;
                  action down ke.keysym acc
              | KEYUP ke ->
                  pressed_keys := KeyMap.remove ke.keysym !pressed_keys;
                  action up ke.keysym acc
              | QUIT ->
                  execute_on_quit ();
                  acc
              | _ -> acc
            end in
            read_events acc
    in
    let actions = read_events [] in
    let actions =
      KeyMap.fold (fun k _ -> action continuous k) !pressed_keys actions in
    let actions =
      KeyMap.fold (action_auto now) !pressed_keys actions in
    List.rev actions @ !instant

  let key_continuous k a = continuous := KeyMap.add k a !continuous
  let key_up k a = up := KeyMap.add k a !up
  let key_down k a = down := KeyMap.add k a !down
  let key_auto ini rep k a = auto := KeyMap.add k (a, ini, rep) !auto

  let reset () = pressed_keys := KeyMap.empty
end
