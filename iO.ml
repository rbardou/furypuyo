open Sdlvideo
open Sdlevent
open Sdlttf

let screen = ref (fun () -> assert false)

let width = ref 0
let height = ref 0

let last_tick = ref 0

let init w h =
  Sdl.init [`TIMER; `VIDEO];
  enable_events
    (make_mask [
       KEYDOWN_EVENT;
       KEYUP_EVENT;
       MOUSEBUTTONDOWN_EVENT;
     ]);
  Sdlttf.init ();
  width := w;
  height := h;
  let the_screen = set_video_mode ~w ~h ~bpp: 32 [`HWSURFACE; `DOUBLEBUF] in
  screen := (fun () -> the_screen);
  last_tick := Sdltimer.get_ticks ()

let frame_delay d =
  let now = Sdltimer.get_ticks () in
  if !last_tick + d > now then
    Sdltimer.delay (d - now + !last_tick);
  last_tick := Sdltimer.get_ticks ()

let screen () = !screen ()

let update () =
  flip (screen ())

let quit = Sdl.quit

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

module Text = struct
  type t = Sdlttf.font

  let load file size = open_font file size

  let write font ?(align = TopLeft) x y txt =
    let (w, h) = Sdlttf.size_text font txt in
    let txt = Sdlttf.render_text_solid font txt ~fg: white in
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
end

module Sprite = struct
  type t = {
    hotx: int;
    hoty: int;
    surface: surface;
  }

  let of_surface ?(align = TopLeft) surface =
    let w, h, _ = surface_dims surface in
    let hotx, hoty = hotxy w h align in
    {
      hotx = hotx;
      hoty = hoty;
      surface = surface;
    }

  let load ?align file =
    of_surface ?align (Sdlloader.load_image file)

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
    let surface =
      create_RGB_surface_format
        (screen ())
        [`HWSURFACE]
        ~w: !width
        ~h: !height
    in
    blit_surface
      ~src: (screen ())
      ~dst: surface
      ~dst_rect: {
        r_x = 0;
        r_y = 0;
        r_w = 0;
        r_h = 0;
      } ();
    of_surface ?align surface
end

module type ACTION = sig
  type t
end

module MakeReader(A: ACTION) = struct
  module Key = struct
    type t = Sdlkey.t
    let compare = compare
  end
  module KeyMap = Map.Make(Key)
  module KeySet = Set.Make(Key)

  let continuous = ref KeyMap.empty
  let up = ref KeyMap.empty
  let down = ref KeyMap.empty

  let pressed_keys = ref KeySet.empty

  let action mapref key acc =
    try KeyMap.find key !mapref :: acc
    with Not_found -> acc

  let read () =
    let rec read_events acc =
      match poll () with
        | None -> acc
        | Some event ->
            let acc = begin match event with
              | KEYDOWN ke ->
                  pressed_keys := KeySet.add ke.keysym !pressed_keys;
                  action down ke.keysym acc
              | KEYUP ke ->
                  pressed_keys := KeySet.remove ke.keysym !pressed_keys;
                  action up ke.keysym acc
              | _ -> acc
            end in
            read_events acc
    in
    let actions = read_events [] in
    let actions = KeySet.fold (action continuous) !pressed_keys actions in
    List.rev actions

  let key_continuous k a = continuous := KeyMap.add k a !continuous
  let key_up k a = up := KeyMap.add k a !up
  let key_down k a = down := KeyMap.add k a !down
end
