(** Representation of Ludo Game Board.

    This module represents the game board used during a game of Ludo. It creates
    an initialized state of the board and makes changes to the player's piece
    position according to moves made by the user interface.*)

type t = Move.t list
(* The type [t] represents a list of Move.t elements. See [Move.mli] for a more
   detailed description of Move.t*)

type board_state = {
  pieces : t;
  player_turn : Player.color;
  game_status : bool;
}
(* The type [board_state] represents the game board, in which [pieces] contains
   the list of pieces for each player in the game, [player_turn] contains the
   color of the player who has the next move, and [game_status] communicates to
   the board if a player wins *)

val string_of_color : Player.color -> string
(* [string_of_piece color] converts color [color] into a string*)

val string_of_piece : Player.piece_num -> string
(* [string_of_piece p] converts piece_num [p] into a string*)

val get_player_turn : board_state -> Player.color
(* [get_player_turn b] returns which player currently has the turn to move their
   piece on the board [b]*)

val get_pieces : board_state -> t
(* [get_pieces b] returns the list of pieces on the board [b] *)

val init_board : t
(* [init_board] represents the initial state of the board, in which every piece
   starts at 0 *)

val start_board : board_state
(*[start_board] represents the initial board state required to start the game.
  It uses [init_board] to represent the initialized state of its pieces, starts
  the game with Red as player one, and initializes game_status to false to
  indicate that no player has won yet*)

val send_back : board_state -> board_state
(* [send_back b] checks board [b] after a piece is moved to see if they landed
   in another players spot. If so, the original player in the spot gets sent
   back to their home position and the current player now occupies that spot*)

val make_turn : board_state -> Command.phrase -> board_state
(* [make_turn b p] Takes in a board state [b] and command [p] (a command string
   contiaining the desired peice) to update the pieces field in [b] after to [p]
   is moved *)

val print_board : t -> string
(* [print_board piece_list] takes record value pieces [piece_list] and converts
   it into a string.*)

val get_equal : t -> Command.phrase list
(*get_equal [m] converts type [t] into a list of phrases.*)

val equal : Command.phrase -> Command.phrase list -> Command.phrase list -> bool
(* equal [p] [b1] [b2] compares the phrase lists [b1] and [b2] and checks for
   equality*)

val find_moved_piece :
  Command.phrase list -> Command.phrase list -> Command.phrase
(* find_moved_piece [p1] [p2] finds the piece that was moved after send_back is
   called *)
