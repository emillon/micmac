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

type state =
  { player_pos : int * int
  ; matrix : obj Matrix.t
  ; rat_pos : int * int
  ; rat_alive : bool
  }

let add_delta (x, y) (dx, dy) = (x+dx, y+dy)

let actors state =
  let rat =
    if state.rat_alive then
      [(state.rat_pos, Rat)]
    else
      []
  in
  [(state.player_pos, Player)]@rat

let actor_at state pos =
  match List.assoc pos (actors state) with
  | exception Not_found -> None
  | actor -> Some actor

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

let interpret_action state = function
  | `Invalid c ->
      begin
        msg "Invalid command : '%c'\n%!" c >>
        Lwt.return state
      end
  | `Move delta ->
      let player_pos = add_delta state.player_pos delta in
      match can_move state player_pos with
      | `Can_move ->
          Lwt.return
            { state with
              player_pos
            }
      | `Bonk ->
        begin
          msg "bonk" >>
          Lwt.return state
        end
      | `Fight _ ->
          begin
            msg "The creature could not do anything" >>
            Lwt.return
              { state with
                rat_alive = false
              }
          end

let add_actors =
  let go m (pos, obj) =
    Matrix.put m pos obj
  in
  List.fold_left go

let display_state state =
  let matrix_with_actors =
    add_actors
      state.matrix
      (actors state)
  in
  print matrix_with_actors;
  Lwt.return_unit

let set_unbuffered () =
  let open Unix in
  let terminfo = tcgetattr stdin in
  let newterminfo = {terminfo with c_icanon = false; c_vmin = 0; c_vtime = 0} in
  at_exit (fun _ -> tcsetattr stdin TCSAFLUSH terminfo);
  tcsetattr stdin TCSAFLUSH newterminfo

let main () =
  let init_state =
    { player_pos = (5, 5)
    ; matrix = level
    ; rat_pos = (7, 7)
    ; rat_alive = true
    }
  in
  set_unbuffered ();
  lwt_forever init_state @@ fun state ->
    display_state state >>
    let%lwt in_c = Lwt_io.read_char Lwt_io.stdin in
    let%lwt new_state = interpret_action state @@ parse_action in_c in
    Lwt.return new_state

let _ = Lwt_main.run @@ main ()
