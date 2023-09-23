(** Representation of a player's state.

    This module represents the position of a player. This includes a player's
    initial homestate, current position, all four of their pieces, and the
    result of their movements. *)

type t = {
  player : Player.t;
  position : (Player.color * Command.phrase * int) list;
  win : bool;
}
(** The abstract type of values representing players state. *)

val init_state : Player.t -> t
(** [init_state st] is the initial state of each player state [st]. In that
    state the player is currently located at their color's home. *)

val current : t -> (Player.color * string * int) list
(* [current st] A record of the current position of player state's [st] piece *)

val end_pos : t -> bool
(* [end_pos st] is the position that each player's peice must be in for the
   player to win the game*)

val dice_roll : unit -> int
(* A 6-sided die is rolled during each player's turn *)

val make_move : t -> string -> (Player.color * string * int) list
(* changes a piece's postion according to the dice roll *)

val update : t -> string -> t
(* this updates the players' position and win status after every move *)

val player_turn : Player.color -> Player.color
(* this keeps track of the which player will go next *)

val print_pieces : t -> string
(* prints the pieces and their location for each player *)
