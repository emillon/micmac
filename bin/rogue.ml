let level : Tile.t Matrix.t =
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
  match Matrix.parse Tile.parse data with
  | Ok x -> x
  | Error e -> failwith e

let print = Matrix.print Tile.print

let rec lwt_forever state f =
  let%lwt new_state = f state in
  lwt_forever new_state f

let display_state state =
  print @@ Game.add_actors state;
  Lwt.return_unit

let set_unbuffered () =
  let open Unix in
  let terminfo = tcgetattr stdin in
  let newterminfo = {terminfo with c_icanon = false; c_vmin = 0; c_vtime = 0} in
  at_exit (fun _ -> tcsetattr stdin TCSAFLUSH terminfo);
  tcsetattr stdin TCSAFLUSH newterminfo

let fresh () =
  Oo.id (object end)

let main () =
  let init_state =
    Game.create_state
      ~matrix:level
      ~actors:
        [ Player.create ~pos:(5, 5)
        ; Rat.create ~pos:(3, 5)
        ; Rat.create ~pos:(7, 7)
        ; Rat.create ~pos:(5, 7)
        ]
  in
  set_unbuffered ();
  lwt_forever init_state @@ fun state ->
    display_state state >>
    let go state actor =
      if Game.actor_alive state actor then
        Actor.action actor state
      else
        Lwt.return state
    in
    Lwt_list.fold_left_s go state (Game.actors state)

let _ = Lwt_main.run @@ main ()
