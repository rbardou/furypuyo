open Block
open Game
open IO
open Puyo
open Cell

let cellw = 20
let cellh = 20
let fieldy = 30

let () = IO.init (cellw*Game.width) (cellh*(Game.height-2) + fieldy)

let load_puyo = Sprite.load
let sprite_puyo_red = load_puyo "data/red.png"
let sprite_puyo_green = load_puyo "data/green.png"
let sprite_puyo_blue = load_puyo "data/blue.png"
let sprite_puyo_yellow = load_puyo "data/yellow.png"
let foreground = Sprite.load "data/foreground.png"
let background = Sprite.load "data/background.png"
let font = Text.load "data/pouyou.ttf" 16

let sprite_of_puyo p =
  match p.color with
    | Red -> sprite_puyo_red
    | Green -> sprite_puyo_green
    | Blue -> sprite_puyo_blue
    | Yellow -> sprite_puyo_yellow

let draw_puyo game puyo x y =
  match game.state with
    | GameOver gos ->
        if game.now - gos.gos_start < 200 then begin
          let h = Matrix.height game.field in
          let d = max 0 (y - h + game.now - gos.gos_start) in
          let dy = d * (d - 1) in
          let dy = dy / 10 in
          Sprite.draw (sprite_of_puyo puyo) (x*20) ((y-2)*20+dy+fieldy)
        end
    | _ ->
        let dx, dy = match puyo.effect with
          | Moving me when game.now < me.me_end ->
              let d = float_of_int (game.now - me.me_start) in
              let dx = (d *. me.me_sx +. me.me_px) *. float_of_int cellw in
              let dy = (d *. me.me_sy +. me.me_py) *. float_of_int cellh in
              int_of_float dx, int_of_float dy
          | _ -> 0, 0
        in
        Sprite.draw (sprite_of_puyo puyo) (x*20+dx) ((y-2)*20+dy+fieldy)

let draw game =
  let draw_puyo = draw_puyo game in
  let draw_falling_puyo p =
    draw_puyo { p with effect = game.inc_effect } in
  Sprite.draw background 0 0;
  begin match game.incb with
    | List1 puyos
    | List2 puyos ->
        List.iter
          (fun (x, y, p) -> draw_falling_puyo p (game.incx+x) (game.incy+y))
          puyos
    | Quad (color, _) ->
        draw_falling_puyo (Puyo.make color) game.incx game.incy;
        draw_falling_puyo (Puyo.make color) (game.incx+1) game.incy;
        draw_falling_puyo (Puyo.make color) game.incx (game.incy+1);
        draw_falling_puyo (Puyo.make color) (game.incx+1) (game.incy+1)
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
  Sprite.draw foreground 0 0;
  let score1 = string_of_int game.score in
  let score2 = match game.state with
    | Delete ds ->
        let a, b = score_to_add game ds.ds_counts in
        Printf.sprintf "+ %d x %d" a b
    | _ ->
        " "
  in
  Text.write font ~align: Left 10 ~color: Sdlvideo.red (fieldy / 3 - 2) score1;
  Text.write font ~align: Left 10 ~color: Sdlvideo.red
    (2 * fieldy / 3 + 2) score2;
  update ()
