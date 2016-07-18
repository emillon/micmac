type 'state t

type 's action = 's t -> 's -> 's Lwt.t

val create : pos:int*int -> tile:Tile.t -> action:'s action -> 's t

val pos : 's t -> int * int

val tile : 's t -> Tile.t

val action : 's t -> 's -> 's Lwt.t

val delete_from_list : 's t -> 's t list -> 's t list

val update_pos_in_list : 's t -> int * int -> 's t list -> 's t list

val exists_in_list : 's t -> 's t list -> bool
