type t =
  | Player
  | Nothing
  | Wall
  | Empty
  | Rat

val parse : char -> t option

val print : t -> char
