(* type move represents an individual piece that a player wants to move on the
   board. It references the player color that the piece belongs, the piece_num,
   and the location of the piece on the board *)
type t = {
  player : Player.color;
  piece : Player.piece_num;
  location : int;
}
(* The abstract type of values representing board states. *)

val get_player : t -> Player.color
(* returns the color of the current player *)

val get_piece : t -> Player.piece_num
(* returns the piece the player is moving *)

val get_loc : t -> int
(* this tells us the location of the player's piece *)

val build : string -> t
(* also us to build the board and intialize it for each piece *)
