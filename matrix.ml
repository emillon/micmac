type 'a t = 'a list list

let matrix_size m =
  (List.length m, List.length (List.hd m))

let rec list_alter l i f =
  match (l, i) with
  | x::xs, 0 -> (f x)::xs
  | x::xs, _ when i < 0 -> invalid_arg "list_alter"
  | x::xs, _ -> x::(list_alter xs (i-1) f)
  | [], _ -> invalid_arg "list_alter"

let put m (i, j) x =
  list_alter m i @@ fun row ->
  list_alter row j @@ fun _ ->
  x

let iteri f m =
  List.iteri (fun i row ->
    List.iteri (fun j x ->
      f (i, j) x
    ) row
  ) m

let foldi f z m =
  let res = ref z in
  iteri (fun pos v ->
    res := f pos v (!res)
  ) m;
  !res

let get m (i, j) =
  List.nth (List.nth m i) j

let rec list_make n x =
  match n with
  | 0 -> []
  | _ when n < 0 -> invalid_arg "list_make"
  | _ -> x::(list_make (n-1) x)

let make (h, w) x =
  list_make h @@ list_make w x

let parse_error (i, j) c =
  Error (Printf.sprintf "Invalid char at (%d, %d): '%c'" i j c)

let string_to_list s =
  Array.to_list @@ Stringext.to_array s

let parse parse_obj ms =
  let m : char list list = List.map string_to_list ms in
  let (h, w) = matrix_size m in
  let initial_char = get m (0, 0) in
  match parse_obj initial_char with
  | None -> parse_error (0, 0) initial_char
  | Some initial_obj ->
      begin
        let go (i, j) c = function
          | Error _ as e -> e
          | Ok st ->
              begin
                match parse_obj c with
                  | Some x -> Ok (put st (i, j) x)
                  | None -> parse_error (i, j) c
              end
        in
        let res = Ok (make (h, w) initial_obj) in
        foldi go res m
      end

let print print_ob matrix =
  List.iter (fun row ->
    List.iter (fun x -> print_char (print_ob x)) row;
    print_newline ()
  ) matrix
