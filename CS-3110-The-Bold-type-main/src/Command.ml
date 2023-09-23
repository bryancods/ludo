open Player

type phrase = string

exception Empty
exception Invalid
exception Malformed

type command =
  | Quit
  | Move of phrase
  | Invalid of phrase
(* this is to be used in the start game func i believe if not then we dont need
   it cause i checked the number of players in the UI *)

type other_command =
  | Yes
  | No

let parse_instructions str : other_command option =
  match str with
  | instruct ->
      if instruct = "no" then Some No
      else if instruct = "yes" then Some Yes
      else None

let parse_player str =
  match str with
  | "red" -> Red
  | "green" -> Green
  | "blue" -> Blue
  | "yellow" -> Yellow
  | _ -> raise Invalid

let parse_piece str =
  match str with
  | "1" -> One
  | "2" -> Two
  | "3" -> Three
  | "4" -> Four
  | _ -> raise Invalid

let check_parse_piece str =
  match str with
  | "1" -> Some One
  | "2" -> Some Two
  | "3" -> Some Three
  | "4" -> Some Four
  | _ -> None

let get_player_piece str =
  let lower_split =
    String.split_on_char ' ' (String.trim (String.lowercase_ascii str))
  in
  match lower_split with
  | player :: piece_num :: t -> piece_num
  | _ -> raise Invalid

let check_get_player_piece str =
  let lower_split =
    String.split_on_char ' ' (String.trim (String.lowercase_ascii str))
  in
  match lower_split with
  | player :: piece_num :: t -> Some piece_num
  | _ -> None

let check_parse_player str =
  match str with
  | "red" -> Some Red
  | "green" -> Some Green
  | "blue" -> Some Blue
  | "yellow" -> Some Yellow
  | _ -> None

let check_get_player str =
  let lower_split =
    String.split_on_char ' ' (String.trim (String.lowercase_ascii str))
  in
  match lower_split with
  | player :: piece_num :: t -> (
      match check_parse_player player with
      | Some s -> Some player
      | None -> None)
  | _ -> None

let check_move (p : phrase) : command =
  if
    check_get_player_piece p = None
    || check_get_player p = None
    || check_parse_piece (get_player_piece p) = None
  then Invalid "Invalid Command"
  else Move p

let parse_find_piece (p : phrase) : int =
  let lower_split =
    String.split_on_char ' ' (String.trim (String.lowercase_ascii p))
  in
  match lower_split with
  | player :: piece_num :: i :: t -> int_of_string i
  | _ -> raise Invalid

let parse_command (cmd : string) : command =
  let cmd_lower_split =
    String.split_on_char ' ' (String.trim (String.lowercase_ascii cmd))
  in
  match cmd_lower_split with
  | h :: t ->
      if h = "quit" || h = "q" then Quit
      else if h = "move" || h = "m" then check_move (String.concat " " t)
      else Invalid "Invalid Command"
  | _ -> Invalid "No command found"
