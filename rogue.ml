type obj =
  | Player
  | Nothing
  | Wall
  | Empty

let parse_obj = function
  | '@' -> Some Player
  | '.' -> Some Nothing
  | '#' -> Some Wall
  | ' ' -> Some Empty
  | _ -> None

let print_obj = function
  | Player -> '@'
  | Nothing -> '.'
  | Wall -> '#'
  | Empty -> ' '

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
  (*| 'h' -> `Go (-1,  0)*)
  (*| 'j' -> `Go ( 0, +1)*)
  (*| 'k' -> `Go ( 0, -1)*)
  (*| 'l' -> `Go (+1,  0)*)
  | c -> `Invalid c

let rec lwt_forever state f =
  let%lwt new_state = f state in
  lwt_forever new_state f

let interpret_action state = function
  | `Invalid c ->
      begin
        Printf.printf "Invalid command : '%c'\n%!" c;
        Lwt.return state
      end

type state =
  { player_pos : int * int
  ; matrix : obj Matrix.t
  }

let display_state state =
  let matrix_with_player = Matrix.put state.matrix state.player_pos Player in
  print matrix_with_player;
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
    }
  in
  set_unbuffered ();
  lwt_forever init_state @@ fun state ->
    display_state state >>
    let%lwt in_c = Lwt_io.read_char Lwt_io.stdin in
    let%lwt new_state = interpret_action state @@ parse_action in_c in
    Lwt.return new_state

let _ = Lwt_main.run @@ main ()
