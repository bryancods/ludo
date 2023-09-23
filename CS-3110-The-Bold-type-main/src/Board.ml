open Player
open State
open Move
open Command

type t = Move.t list

let string_of_color (color : Player.color) : string =
  match color with
  | Red -> "red"
  | Green -> "green"
  | Blue -> "blue"
  | Yellow -> "yellow"

let string_of_piece (p : piece_num) : string =
  match p with
  | One -> "1"
  | Two -> "2"
  | Three -> "3"
  | Four -> "4"

type board_state = {
  pieces : t;
  player_turn : Player.color;
  game_status : bool;
}

let get_player_turn (b : board_state) = b.player_turn
let get_pieces (b : board_state) = b.pieces

let init_board : t =
  [
    build "red 1";
    build "red 2";
    build "red 3";
    build "red 4";
    build "green 1";
    build "green 2";
    build "green 3";
    build "green 4";
    build "blue 1";
    build "blue 2";
    build "blue 3";
    build "blue 4";
    build "yellow 1";
    build "yellow 2";
    build "yellow 3";
    build "yellow 4";
  ]

let start_board =
  { pieces = init_board; player_turn = Red; game_status = false }

(* helper function [convert_player_pieces] converts them into (color * string *
   int) *)
let convert_player_pieces (m : Move.t) : color * string * int =
  let player = m.player in
  let piece = string_of_color m.player ^ " " ^ string_of_piece m.piece in
  let location = m.location in
  (player, piece, location)

(* helper function [find_player_peices] finds the pieces of the current player
   on the board and calls [convert_player_pieces] to create the state.t input
   required for make_move*)
let rec find_player_pieces (m_list : t) (b : board_state) :
    (color * string * int) list =
  match m_list with
  | h :: t ->
      if get_player h = get_player_turn b then
        convert_player_pieces h :: find_player_pieces t b
      else find_player_pieces t b
  | _ -> []

(*helper function [updated_player_pieces] produces the updated list of new
  pieces for the current player's turn after a move *)
let rec updated_player_pieces (updated_pieces : (color * string * int) list) =
  match updated_pieces with
  | (c, s, i) :: t ->
      {
        player = c;
        piece = Command.parse_piece (Command.get_player_piece s);
        location = i;
      }
      :: updated_player_pieces t
  | [] -> []

(* helper function [delete_player_pieces] creates list of pieces with old
   locations deleted *)
let rec delete_player_pieces (pieces : t) (b : board_state) =
  match pieces with
  | [] -> []
  | h :: t ->
      if h.player != b.player_turn then h :: delete_player_pieces t b
      else delete_player_pieces t b

(* helper function [current_moves] creates list of locations that the current
   player has pieces on*)
let rec current_moves (l : (color * phrase * int) list) : int list =
  match l with
  | [] -> []
  | (c, p, i) :: t -> i :: current_moves t

(* helper function [dont_move] indicates which pieces to keep in send_back*)
let rec dont_move (l : t) (b : board_state) : t =
  match l with
  | [] -> []
  | h :: t ->
      if h.player = b.player_turn then h :: dont_move t b else dont_move t b

(* helper function [call_send_back] sends back player *)
let rec call_send_back (other : t) (b : board_state) : t =
  match other with
  | [] -> []
  | { player = p; piece = pie; location = loc } :: t ->
      if List.mem loc (current_moves (find_player_pieces b.pieces b)) && loc > 0
      then { player = p; piece = pie; location = 0 } :: call_send_back t b
      else { player = p; piece = pie; location = loc } :: call_send_back t b

let send_back (b : board_state) =
  let other = delete_player_pieces b.pieces b in
  let pie = call_send_back other b in
  let new_pie = dont_move b.pieces b @ pie in
  { pieces = new_pie; player_turn = get_player_turn b; game_status = false }

let make_turn (b : board_state) (p : Command.phrase) =
  let play =
    { color = b.player_turn; pieces = find_player_pieces b.pieces b }
  in
  let st = { player = play; position = play.pieces; win = false } in
  let move = update st p in
  let part_one = updated_player_pieces move.position in
  let part_two = delete_player_pieces b.pieces b in
  let new_pieces = part_one @ part_two in
  let check_send_back =
    { pieces = new_pieces; player_turn = b.player_turn; game_status = move.win }
  in
  let check = send_back check_send_back in

  {
    pieces = check.pieces;
    player_turn = State.player_turn b.player_turn;
    game_status = move.win;
  }

let rec print_board (piece_list : t) : string =
  match piece_list with
  | [] -> ""
  | h :: t ->
      string_of_color (Move.get_player h)
      ^ " "
      ^ string_of_piece (Move.get_piece h)
      ^ " "
      ^ string_of_int (Move.get_loc h)
      ^ "\n" ^ print_board t

let rec remove r li =
  match li with
  | h :: t -> if h = r then remove r t else h :: remove r t
  | [] -> []

let removed_piece r = r

let rec get_equal (m : t) : phrase list =
  match m with
  | { player = play; piece = pie; location = i } :: t ->
      (string_of_color play ^ " " ^ string_of_piece pie ^ " " ^ string_of_int i)
      :: get_equal t
  | _ -> []

let equal p b1 b2 =
  let p1 = remove p b1 in
  let p2 = remove p b2 in
  let rec find_equal (p1 : phrase list) (p2 : phrase list) : bool =
    match p1 with
    | h :: t ->
        if List.mem h p2 then
          let new_p2 = remove h p2 in
          find_equal t new_p2
        else false
    | [] -> true
  in
  find_equal p1 p2

let rec find_moved_piece (p1 : phrase list) (p2 : phrase list) =
  match p1 with
  | h :: t ->
      if List.mem h p2 then
        let new_p2 = remove h p2 in
        find_moved_piece t new_p2
      else h
  | [] -> ""
