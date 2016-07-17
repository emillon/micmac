type obj =
  | Player
  | Nothing
  | Wall
  | Empty
  | Rat

let parse_obj = function
  | '@' -> Some Player
  | '.' -> Some Nothing
  | '#' -> Some Wall
  | ' ' -> Some Empty
  | 'r' -> Some Rat
  | _ -> None

let print_obj = function
  | Player -> '@'
  | Nothing -> '.'
  | Wall -> '#'
  | Empty -> ' '
  | Rat -> 'r'

let level : obj Matrix.t =
  let data = 
    [ "####             "
    ; "#..#             "
    ; "#..#             "
    ; "#..##############"
    ; "#...............#"
    ; "#...............#"
    ; "#...............#"
    ; "#...............#"
    ; "#...............#"
    ; "#################"
    ]
  in
  match Matrix.parse parse_obj data with
  | Ok x -> x
  | Error e -> failwith e

let print = Matrix.print print_obj

let parse_action = function
  | 'h' -> `Move (-1,  0)
  | 'j' -> `Move ( 0, +1)
  | 'k' -> `Move ( 0, -1)
  | 'l' -> `Move (+1,  0)
  | c -> `Invalid c

let rec lwt_forever state f =
  let%lwt new_state = f state in
  lwt_forever new_state f

type 'state actor =
  { id : int
  ; pos : int * int
  ; obj : obj
  ; action : 'state -> 'state Lwt.t
  }

type state =
  { matrix : obj Matrix.t
  ; actors : state actor list
  }

let add_delta (x, y) (dx, dy) = (x+dx, y+dy)

let rec first_matching p = function
  | [] -> None
  | x::xs ->
    begin
      match p x with
      | Some y -> Some y
      | None -> first_matching p xs
    end

let actor_at state pos =
  let p actor =
    if actor.pos = pos then
      Some actor
    else
      None
  in
  first_matching p state.actors

let can_move state pos =
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

let msg fmt =
  let k s =
    print_endline s;
    Lwt.return_unit
  in
  Printf.kprintf k fmt

let rec delete_actor actor_to_delete = function
  | actor::actors when actor.id = actor_to_delete.id -> actors
  | actor::actors -> actor::delete_actor actor_to_delete actors
  | [] -> []

let kill_actor state actor =
  { state with
    actors = delete_actor actor state.actors
  }

let player state =
  let p actor =
    if actor.obj = Player then
      Some actor
    else
      None
  in
  match first_matching p state.actors with
  | Some x -> x
  | None -> assert false

let rec update_one p modify = function
  | [] -> invalid_arg "update_one"
  | x::xs when p x -> (modify x)::xs
  | x::xs -> x::(update_one p modify xs)


let update_player_pos state player_pos =
  let actors =
    update_one
      (fun actor -> actor.obj = Player)
      (fun actor -> { actor with pos = player_pos })
      state.actors
  in
  { state with actors }

let interpret_action state = function
  | `Invalid c ->
      begin
        msg "Invalid command : '%c'\n%!" c >>
        Lwt.return state
      end
  | `Move delta ->
      let player = player state in
      let player_pos = add_delta player.pos delta in
      match can_move state player_pos with
      | `Can_move ->
          Lwt.return @@ update_player_pos state player_pos
      | `Bonk ->
        begin
          msg "bonk" >>
          Lwt.return state
        end
      | `Fight actor ->
          begin
            msg "The creature could not do anything" >>
            Lwt.return @@ kill_actor state actor
          end

let add_actors =
  let go m actor =
    Matrix.put m actor.pos actor.obj
  in
  List.fold_left go

let display_state state =
  let matrix_with_actors =
    add_actors
      state.matrix
      state.actors
  in
  print matrix_with_actors;
  Lwt.return_unit

let set_unbuffered () =
  let open Unix in
  let terminfo = tcgetattr stdin in
  let newterminfo = {terminfo with c_icanon = false; c_vmin = 0; c_vtime = 0} in
  at_exit (fun _ -> tcsetattr stdin TCSAFLUSH terminfo);
  tcsetattr stdin TCSAFLUSH newterminfo

let fresh () =
  Oo.id (object end)

let player_action state =
  let%lwt in_c = Lwt_io.read_char Lwt_io.stdin in
  let%lwt new_state = interpret_action state @@ parse_action in_c in
  Lwt.return new_state

let noop_action state =
  Lwt.return state

let main () =
  let init_state =
    { matrix = level
    ; actors =
      [ { id = fresh () ; pos = (5, 5) ; obj = Player ; action = player_action }
      ; { id = fresh () ; pos = (3, 5) ; obj = Rat ; action = noop_action }
      ]
    }
  in
  set_unbuffered ();
  lwt_forever init_state @@ fun state ->
    display_state state >>
    let go state actor =
      actor.action state
    in
    Lwt_list.fold_left_s go state state.actors

let _ = Lwt_main.run @@ main ()
