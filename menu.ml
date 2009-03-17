open Sprites

module MenuAction = struct
  type t = Up | Down | Return
end
module MenuReader = IO.MakeReader(MenuAction)

open MenuAction

let () =
  MenuReader.key_auto 500 100 Sdlkey.KEY_UP Up;
  MenuReader.key_auto 500 100 Sdlkey.KEY_DOWN Down;
  MenuReader.key_down Sdlkey.KEY_RETURN Return

let sprite_puyo = IO.Sprite.align sprite_puyo_red IO.Center

let string_choices choices =
  let choices = Array.of_list choices in
  MenuReader.reset ();
  let result = ref (fun () -> assert false) in
  let return x =
    result := (fun () -> x);
    raise Exit
  in
  let background = IO.Sprite.screenshot () in
  let choice = ref 0 in
  let count = Array.length choices in
  IO.timer_start ();
  let choice_y i = (i + 1) * screen_height / (count + 1) in
  let choice_x = screen_width / 2 in
  let choice_radius =
    Array.init count
      (fun i ->
         fst (IO.Text.size font (fst choices.(i))) / 2 +
           IO.Sprite.width sprite_puyo)
  in
  let puyo_x = ref (float_of_int choice_radius.(!choice)) in
  let puyo_y = ref (float_of_int (choice_y !choice)) in
  try
    while true do
      if IO.frame_delay 10 then begin
        IO.Sprite.draw background 0 0;
        Array.iteri
          (fun i (str, _) ->
             let y = choice_y i in
             IO.Text.write font ~align: IO.Center choice_x y str)
          choices;

        IO.Sprite.draw sprite_puyo (choice_x - int_of_float !puyo_x)
          (int_of_float !puyo_y);
        IO.Sprite.draw sprite_puyo (choice_x + int_of_float !puyo_x)
          (int_of_float !puyo_y);
      
        IO.update ()
      end;

      List.iter
        (function
           | Up ->
               decr choice;
               if !choice < 0 then choice := count - 1
           | Down ->
               incr choice;
               if !choice >= count then choice := 0
           | Return ->
               return (snd (choices.(!choice))))
        (MenuReader.read ());

      puyo_y := !puyo_y +. (float_of_int (choice_y !choice) -. !puyo_y) /. 10.;
      puyo_x :=
        !puyo_x +. (float_of_int choice_radius.(!choice) -. !puyo_x) /. 10.;
    done;
    assert false
  with Exit ->
    !result ()
