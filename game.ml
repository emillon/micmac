let rec first_matching p = function
  | [] -> None
  | x::xs ->
    begin
      match p x with
      | Some y -> Some y
      | None -> first_matching p xs
    end

type state =
  { matrix : Tile.t Matrix.t
  ; actors : state Actor.t list
  }

let actors s = s.actors

let create_state ~matrix ~actors =
  { matrix
  ; actors
  }

let actor_at state pos =
  let p actor =
    if Actor.pos actor = pos then
      Some actor
    else
      None
  in
  first_matching p state.actors

let can_move state pos =
  let open Tile in
  match actor_at state pos with
  | Some actor ->
    `Fight actor
  | None ->
      begin
        match Matrix.get state.matrix pos with
        | Wall -> `Bonk
        | Nothing -> `Can_move
        | Player -> assert false
        | Empty -> assert false
        | Rat -> assert false
      end

let kill_actor state actor =
  { state with
    actors = Actor.delete_from_list actor state.actors
  }

let player state =
  let p actor =
    if Actor.tile actor = Tile.Player then
      Some actor
    else
      None
  in
  match first_matching p state.actors with
  | Some x -> x
  | None -> assert false

let update_actor_pos state actor_to_update new_pos =
  let actors =
    Actor.update_pos_in_list actor_to_update new_pos state.actors
  in
  { state with actors }

let add_actors state =
  let go m actor =
    Matrix.put m (Actor.pos actor) (Actor.tile actor)
  in
  List.fold_left go state.matrix state.actors

let actor_alive state actor_to_check =
  Actor.exists_in_list actor_to_check state.actors
