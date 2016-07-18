type t =
  | Player
  | Nothing
  | Wall
  | Empty
  | Rat

let parse = function
  | '@' -> Some Player
  | '.' -> Some Nothing
  | '#' -> Some Wall
  | ' ' -> Some Empty
  | 'r' -> Some Rat
  | _ -> None

let print = function
  | Player -> '@'
  | Nothing -> '.'
  | Wall -> '#'
  | Empty -> ' '
  | Rat -> 'r'

