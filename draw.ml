open Block
open Game
open IO
open Puyo

let () = IO.init 800 600

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

let draw_puyo puyo x y =
  Sprite.draw (sprite_of_puyo puyo) (x*20) (y*20)

let draw game =
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
  update ()
