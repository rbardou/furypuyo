open Block
open Game
open IO
open Puyo
open Cell

let cellw = 20
let cellh = 20
let fieldy = 30
let next_block1_x = cellw*6+15
let next_block1_y = fieldy+15
let next_block2_x = cellw*6+15
let next_block2_y = fieldy+30+2*cellh

let () = IO.init (cellw*8+30) (cellh*12 + fieldy)

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

let draw_field_puyo p x y y_offset =
  Sprite.draw
    (sprite_of_puyo p)
    (field_puyo_x x)
    (field_puyo_y y + cellh * y_offset / Game.smooth_factor)

let block_puyos = function
  | List0 l | List1 l | List2 l -> l
  | Quad (c, _) ->
      let p = Puyo.make c in
      [ 0, 0, p; 0, 1, p; 1, 0, p; 1, 1, p ]

let draw_block block x y =
  List.iter
    (fun (x', y', p) -> draw_puyo p (x + cellw * x') (y + cellh * y'))
    (block_puyos block)

let draw_incoming game is =
  let x = field_puyo_x is.inc_x in
  let y = field_puyo_y 0 + cellh * is.inc_y / Game.smooth_factor in
  draw_block is.inc_block x y

let draw_falling game fs =
  let draw_puyo p x y =
    draw_puyo p
      (field_puyo_x x)
      (field_puyo_y y + cellh * fs.f_y / Game.smooth_factor)
  in
  List.iter (fun (x, y, p) -> draw_puyo p x y) fs.f_puyos

let draw game =
  Sprite.draw background 0 0;
  let y_offset = ref 0 in
  begin match game.state with
    | Incoming is -> draw_incoming game is
    | Falling fs -> draw_falling game fs
    | GameOver gos -> y_offset := gos.go_y
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
              draw_field_puyo puyo x y !y_offset
    done;
  done;
  begin match game.next_blocks with
    | b1 :: b2 :: _ ->
        draw_block b1 next_block1_x next_block1_y;
        draw_block b2 next_block2_x next_block2_y
    | _ -> ()
  end;
  Sprite.draw foreground 0 0;
  Text.write font ~color: Sdlvideo.red 5 1 (string_of_int game.score);
  begin match game.state with
    | Popping ps ->
        Text.write font ~color: Sdlvideo.red 5 14
          (Printf.sprintf "+ %d x %d" ps.pop_score_base ps.pop_score_mult);
    | _ -> ()
  end;
  update ()
