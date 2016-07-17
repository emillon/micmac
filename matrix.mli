type 'a t

val parse : (char -> 'a option) -> string list -> ('a t, string) result

val print : ('a -> char) -> 'a t -> unit

val put : 'a t -> (int * int) -> 'a -> 'a t
