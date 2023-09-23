type color =
  | Red
  | Green
  | Blue
  | Yellow
(* The type [color] represents a player's color corresponding to the number of
   players in the game. [Player One -> Red, Player Two -> Green, Player Three ->
   Blue, Player Four -> Yellow] *)

type t = {
  color : color;
  pieces : (color * string * int) list;
}
(** The abstract type of values representing players. *)

type piece_num =
  | One
  | Two
  | Three
  | Four
(* represents the number of each piece *)

val make : color -> t
(* allows us to make an object of type t *)

val home : t -> (color * string * int) list
(* for each player, all of their pieces start in position 0 *)

val over : t -> (color * string * int) list
(* for when all of a player's pieces reaches position 56 *)

val get_color : t -> color
(* this gets the color of each player *)
