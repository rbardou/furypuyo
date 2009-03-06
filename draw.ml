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

let field_puyo_x x = cellw * x
let field_puyo_y y = cellh * (y - 2) + fieldy

let draw_field_puyo p x y =
  Sprite.draw (sprite_of_puyo p) (field_puyo_x x) (field_puyo_y y)

let draw_incoming game is =
  let draw_puyo p x y =
    draw_puyo p
      (field_puyo_x (x + is.inc_x))
      (field_puyo_y y + cellh * is.inc_y / Game.smooth_factor)
  in
  let block = match is.inc_block with
    | List0 l | List1 l | List2 l -> l
    | Quad (c, _) ->
        let p = Puyo.make c in
        [ 0, 0, p; 0, 1, p; 1, 0, p; 1, 1, p ]
  in
  List.iter (fun (x, y, p) -> draw_puyo p x y) block

let draw_falling game fs =
  let draw_puyo p x y =
    draw_puyo p
      (field_puyo_x x)
      (field_puyo_y y + cellh * fs.f_y / Game.smooth_factor)
  in
  List.iter (fun (x, y, p) -> draw_puyo p x y) fs.f_puyos

let draw game =
  Sprite.draw background 0 0;
  begin match game.state with
    | Incoming is -> draw_incoming game is
    | Falling fs -> draw_falling game fs
    | _ -> ()
  end;
  let hidden = match game.state with
    | Popping ps ->
        if game.now / 2 mod 2 = 0 then ps.pop_puyos else []
    | _ -> []
  in
  for x = 0 to Matrix.width game.field - 1 do
    for y = 0 to Matrix.height game.field - 1 do
      match (Matrix.get game.field x y).puyo with
        | None -> ()
        | Some puyo ->
            if not (List.mem (x, y) hidden) then
              draw_field_puyo puyo x y
    done;
  done;
  Sprite.draw foreground 0 0;
  update ()
