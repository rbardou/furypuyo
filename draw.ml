open Block
open Game
open IO
open Puyo
open Cell

let cellw = 20
let cellh = 20
let fieldy = 30

let () = IO.init (cellw*6) (cellh*12 + fieldy)

let load_puyo = Sprite.load
let sprite_puyo_red = load_puyo "data/red.png"
let sprite_puyo_green = load_puyo "data/green.png"
let sprite_puyo_blue = load_puyo "data/blue.png"
let sprite_puyo_yellow = load_puyo "data/yellow.png"
let sprite_puyo_gray = load_puyo "data/gray.png"
let foreground = Sprite.load "data/foreground.png"
let background = Sprite.load "data/background.png"
let font = Text.load "data/pouyou.ttf" 16

let sprite_of_puyo p =
  match p.color with
    | Red -> sprite_puyo_red
    | Green -> sprite_puyo_green
    | Blue -> sprite_puyo_blue
    | Yellow -> sprite_puyo_yellow
    | Gray -> sprite_puyo_gray

let draw_puyo p =
  Sprite.draw (sprite_of_puyo p)

let draw_incoming game is =
  let draw_puyo p x y =
    draw_puyo p
      (cellw * (x + is.inc_x))
      (cellh * (y - 2) + cellh * is.inc_y / Game.smooth_factor + fieldy)
  in
  let block = match is.inc_block with
    | List0 l | List1 l | List2 l -> l
    | Quad (c, _) ->
        let p = Puyo.make c in
        [ 0, 0, p; 0, 1, p; 1, 0, p; 1, 1, p ]
  in
  List.iter (fun (x, y, p) -> draw_puyo p x y) block

let draw game =
  Sprite.draw background 0 0;
  begin match game.state with
    | Incoming is -> draw_incoming game is
    | _ -> ()
  end;
  update ()
(*  Sprite.draw foreground 0 0;*)
