type state

val actors : state -> state Actor.t list

val player : state -> state Actor.t

val create_state : matrix:Tile.t Matrix.t -> actors:state Actor.t list -> state

val add_actors : state -> Tile.t Matrix.t

val actor_alive : state -> state Actor.t -> bool

val can_move : state -> Pos.t -> [`Bonk | `Can_move | `Fight of state Actor.t]

val update_actor_pos : state -> state Actor.t -> Pos.t -> state

val kill_actor : state -> state Actor.t -> state
