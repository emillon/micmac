type 'state t =
  { id : int
  ; pos : int * int
  ; tile : Tile.t
  ; action : 'state action
  }

and 's action = 's t -> 's -> 's Lwt.t

let fresh () =
  Oo.id (object end)

let create ~pos ~tile ~action =
  { id = fresh ()
  ; pos
  ; tile
  ; action
  }

let pos act =
  act.pos

let tile act =
  act.tile

let action act =
  act.action act

let rec delete_from_list actor_to_delete = function
  | actor::actors when actor.id = actor_to_delete.id -> actors
  | actor::actors -> actor::delete_from_list actor_to_delete actors
  | [] -> []

let rec update_one p modify = function
  | [] -> invalid_arg "update_one"
  | x::xs when p x -> (modify x)::xs
  | x::xs -> x::(update_one p modify xs)

let update_pos_in_list actor_to_update new_pos actor_list =
  update_one
    (fun actor -> actor.id = actor_to_update.id)
    (fun actor -> { actor with pos = new_pos })
    actor_list

let exists_in_list actor_to_check actors =
  List.exists
    (fun actor -> actor.id = actor_to_check.id)
    actors
