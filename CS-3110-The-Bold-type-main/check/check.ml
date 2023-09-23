open Game

module type PlayerSig = sig
  type color
  type t

  val home : t -> (color * string * int) list
  val over : t -> (color * string * int) list
end

module PlayerCheck : PlayerSig = Player

module type BoardSig = sig
  type t
  type board_state

  val string_of_color : Player.color -> string
  val string_of_piece : Player.piece_num -> string
  val get_player_turn : board_state -> Player.color
  val get_pieces : board_state -> t
  val init_board : t
  val start_board : board_state
  val send_back : board_state -> board_state
  val make_turn : board_state -> Command.phrase -> board_state
  val print_board : t -> string

  val equal :
    Command.phrase -> Command.phrase list -> Command.phrase list -> bool

  val get_equal : t -> Command.phrase list

  val find_moved_piece :
    Command.phrase list -> Command.phrase list -> Command.phrase
end

module BoardSig : BoardSig = Board

module type CommandSig = sig
  type phrase = string

  exception Empty
  exception Invalid
  exception Malformed

  type command =
    | Quit
    | Move of phrase
    | Invalid of string

  type other_command =
    | Yes
    | No

  val parse_instructions : string -> other_command option
  val parse_player : string -> Player.color
  val parse_piece : string -> Player.piece_num
  val parse_command : string -> command
  val check_get_player : phrase -> phrase option
  val parse_find_piece : phrase -> int
end

module CommandCheck : CommandSig = Command

module type MoveSig = sig
  type t

  val get_player : t -> Player.color
  val get_piece : t -> Player.piece_num
  val get_loc : t -> int
  val build : string -> t
end

module MoveCheck : MoveSig = Move

module type StateSig = sig
  type t

  val init_state : Player.t -> t
  val dice_roll : unit -> int
  val make_move : t -> string -> (Player.color * string * int) list
  val current : t -> (Player.color * string * int) list
  val end_pos : t -> bool
end

module StateCheck : StateSig = State
