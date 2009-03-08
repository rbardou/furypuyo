open Game

(** in frames (1/100 seconds) *)
let build_delay = 2000
let link_delay = 100

type cpu = {
  start: int; (** time the chain started *)
  chain: int; (** link number in the current chain *)
  level: int; (** current level *)
  next_level: int; (** remaining chains in current level *)
}

let start = {
  start = build_delay;
  chain = 0;
  level = 1;
  next_level = 2;
}

let finish game cpu =
  let game =
    { game with
        garbage_incoming = 0;
        garbage_ready = game.garbage_ready + game.garbage_incoming }
  in
  let level, next_level =
    if cpu.next_level <= 1 then
      cpu.level + 1, 3
    else
      cpu.level, cpu.next_level - 1
  in
  let cpu =
    { start = game.now + build_delay;
      chain = 0;
      level = level;
      next_level = next_level }
  in
  game, cpu

let send_link game cpu =
  let chain = cpu.chain + 1 in
  let cpu = { cpu with chain = chain } in
  let garb = ceil_div (40 * (chain_mult game chain + 4)) 120 in
  let game = { game with garbage_incoming = game.garbage_incoming + garb } in
  if chain >= cpu.level then
    finish game cpu
  else
    game, cpu

let think game cpu =
  match game.state with
    | GameOver _ -> game, cpu
    | _ ->
        if game.now >= cpu.start then
          if (cpu.start - game.now) mod link_delay = 0 then
            send_link game cpu
          else
            game, cpu
        else
          game, cpu
