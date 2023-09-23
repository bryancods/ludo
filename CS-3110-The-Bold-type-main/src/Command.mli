type phrase = string

exception Empty
exception Invalid
exception Malformed

type command =
  | Quit
  | Move of phrase
  | Invalid of phrase
(* type [command] determines whether the game will quit or play based on the
   user's input. If the user writes an unrecognized command it will be invalid*)

type other_command =
  | Yes
  | No
(* type [other command] determines whether the game's instructions will open
   based on the user's input *)

val parse_instructions : string -> other_command option
(* if the user's input is no the instructions will not open. if the user's input
   is yes the instuctions will open. if the user's input is capitalized or
   includes anything besides "yes" or "no" Malformed will be raised *)

val parse_player : string -> Player.color
(* this tells us the player that is currently moving *)

val check_get_player : phrase -> phrase option
(* check_get_player [str] checks for the presence of a player in a phrase*)

val parse_find_piece : phrase -> int
(* parse_find_piece [str] converts the phrase of a location into an int *)

val get_player_piece : phrase -> phrase
(* gets the pieces of the current player *)

val parse_piece : string -> Player.piece_num
(* piece_num represents the number of the piece the user chooses to move *)

val parse_command : string -> command
(*Parse command to indicate if the user wants to quit or make a move. If the
  command is not valid it is Invalid*)
