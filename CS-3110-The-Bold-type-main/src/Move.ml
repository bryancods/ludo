open Player

type t = {
  player : Player.color;
  piece : Player.piece_num;
  location : int;
}

let get_player m = m.player
let get_piece m = m.piece
let get_loc m = m.location

let build str =
  match String.split_on_char ' ' str with
  | [] -> failwith "Invalid string"
  | x :: y :: lst ->
      {
        player = Command.parse_player x;
        piece = Command.parse_piece y;
        location = 0;
      }
  | _ ->
      print_string str;
      failwith "Keep Going"
