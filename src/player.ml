let msg fmt =
  let k s =
    print_endline s;
    Lwt.return_unit
  in
  Printf.kprintf k fmt

let interpret_action state = function
  | `Invalid c ->
      begin
        msg "Invalid command : '%c'\n%!" c >>
        Lwt.return state
      end
  | `Move delta ->
      let player = Game.player state in
      let player_pos = Pos.add_delta (Actor.pos player) delta in
      match Game.can_move state player_pos with
      | `Can_move ->
          Lwt.return @@ Game.update_actor_pos state player player_pos
      | `Bonk ->
        begin
          msg "bonk" >>
          Lwt.return state
        end
      | `Fight actor ->
          begin
            msg "The creature could not do anything" >>
            Lwt.return @@ Game.kill_actor state actor
          end

let parse_action = function
  | 'h' -> `Move (-1,  0)
  | 'j' -> `Move ( 0, +1)
  | 'k' -> `Move ( 0, -1)
  | 'l' -> `Move (+1,  0)
  | 'y' -> `Move (-1, -1)
  | 'u' -> `Move (+1, -1)
  | 'b' -> `Move (-1, +1)
  | 'n' -> `Move (+1, +1)
  | c -> `Invalid c

let action _actor state =
  let%lwt in_c = Lwt_io.read_char Lwt_io.stdin in
  let%lwt new_state = interpret_action state @@ parse_action in_c in
  Lwt.return new_state

let create ~pos =
  Actor.create ~pos ~tile:Tile.Player ~action
