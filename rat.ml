let rat_action actor state =
  let delta =
    match Random.int 4 with
    | 0 -> ( 0, -1)
    | 1 -> ( 0, +1)
    | 2 -> (-1,  0)
    | _ -> (+1,  0)
  in
  let new_pos = Pos.add_delta (Actor.pos actor) delta in
  match Game.can_move state new_pos with
  | `Can_move -> Lwt.return @@ Game.update_actor_pos state actor new_pos
  | `Bonk -> Lwt.return state
  | `Fight _ -> Lwt.return state

let create ~pos =
  Actor.create ~pos ~tile:Tile.Rat ~action:rat_action
