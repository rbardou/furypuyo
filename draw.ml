open Block
open Game
open IO
open Puyo
open Cell

let cellw = 20
let cellh = 20

let () = IO.init (cellw*Game.width) (cellh*(Game.height-2))

let load_puyo = Sprite.load
let sprite_puyo_red = load_puyo "data/red.png"
let sprite_puyo_green = load_puyo "data/green.png"
let sprite_puyo_blue = load_puyo "data/blue.png"
let sprite_puyo_yellow = load_puyo "data/yellow.png"
let background = Sprite.screenshot ()

let sprite_of_puyo p =
  match p.color with
    | Red -> sprite_puyo_red
    | Green -> sprite_puyo_green
    | Blue -> sprite_puyo_blue
    | Yellow -> sprite_puyo_yellow

let draw_puyo now puyo x y =
  let dx, dy = match puyo.effect with
    | Moving me when now < me.me_end ->
        let d = float_of_int (now - me.me_start) in
        let dx = (d *. me.me_sx +. me.me_px) *. float_of_int cellw in
        let dy = (d *. me.me_sy +. me.me_py) *. float_of_int cellh in
        int_of_float dx, int_of_float dy
    | _ -> 0, 0
  in
  Sprite.draw (sprite_of_puyo puyo) (x*20+dx) ((y-2)*20+dy)

let draw game =
  let draw_puyo = draw_puyo game.now in
  Sprite.draw background 0 0;
  begin match game.incb with
    | List1 puyos
    | List2 puyos ->
        List.iter
          (fun (x, y, p) -> draw_puyo p (game.incx+x) (game.incy+y))
          puyos
    | Quad (color, _) ->
        draw_puyo (Puyo.make color) game.incx game.incy;
        draw_puyo (Puyo.make color) (game.incx+1) game.incy;
        draw_puyo (Puyo.make color) game.incx (game.incy+1);
        draw_puyo (Puyo.make color) (game.incx+1) (game.incy+1)
  end;
  let hidden = match game.state with
    | Delete ds ->
        if ds.ds_delay / 2 mod 2 = 0 then ds.ds_cells else []
    | _ -> []
  in
  let draw_puyo puyo x y =
    if not (List.mem (x, y) hidden) then
      draw_puyo puyo x y
    else ()
  in
  for x = 0 to Matrix.width game.field - 1 do
    for y = 0 to Matrix.height game.field - 1 do
      match (Matrix.get game.field x y).puyo with
        | None -> ()
        | Some puyo -> draw_puyo puyo x y
    done;
  done;
  update ()
