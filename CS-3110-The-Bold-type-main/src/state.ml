open Player
open Command

exception Incomplete of string

type t = {
  player : Player.t;
  position : (color * phrase * int) list;
  win : bool;
}

let init_state player = { player; position = home player; win = false }
let current st = st.position
let get_player st = st.player
let end_pos st = if st.position = over st.player then true else false
let dice_roll () : int = Random.int 5

let roll d =
  match d with
  | 0 -> d + 1
  | 1 -> d + 1
  | 2 -> d + 1
  | 3 -> d + 1
  | 4 -> d + 1
  | 5 ->
      let roll_again = Random.int 5 in
      d + roll_again + 1
  | _ -> failwith "Invalid roll!"

let dice () = roll (dice_roll ())

let find_piece st piece =
  let inc = dice () in
  let lst = current st in
  let rec finder l pie =
    match l with
    | (c, p, loc) :: t ->
        if p = piece then (c, p, loc + inc) else finder t piece
    | [] ->
        failwith
          "Piece doesn't exist! Please make sure your entry is in the proper \
           format."
  in
  ANSITerminal.print_string [ ANSITerminal.Bold ]
    ("You rolled a: " ^ string_of_int inc ^ "\n");
  finder lst piece

let remove_old st piece = List.filter (fun (_, p, _) -> piece <> p) (current st)

let make_move st piece =
  match get_color st.player with
  | Red -> (
      match parse_piece (get_player_piece piece) with
      | One -> find_piece st piece :: remove_old st piece
      | Two -> find_piece st piece :: remove_old st piece
      | Three -> find_piece st piece :: remove_old st piece
      | Four -> find_piece st piece :: remove_old st piece)
  | Green -> (
      match parse_piece (get_player_piece piece) with
      | One -> find_piece st piece :: remove_old st piece
      | Two -> find_piece st piece :: remove_old st piece
      | Three -> find_piece st piece :: remove_old st piece
      | Four -> find_piece st piece :: remove_old st piece)
  | Blue -> (
      match parse_piece (get_player_piece piece) with
      | One -> find_piece st piece :: remove_old st piece
      | Two -> find_piece st piece :: remove_old st piece
      | Three -> find_piece st piece :: remove_old st piece
      | Four -> find_piece st piece :: remove_old st piece)
  | Yellow -> (
      match parse_piece (get_player_piece piece) with
      | One -> find_piece st piece :: remove_old st piece
      | Two -> find_piece st piece :: remove_old st piece
      | Three -> find_piece st piece :: remove_old st piece
      | Four -> find_piece st piece :: remove_old st piece)

let update st piece =
  let up =
    { player = st.player; position = make_move st piece; win = st.win }
  in
  { player = up.player; position = up.position; win = end_pos up }

let player_turn color =
  match color with
  | Red -> Green
  | Green -> Blue
  | Blue -> Yellow
  | Yellow -> Red

let print_pieces st =
  let pos = current st in
  let rec searched lst =
    match lst with
    | (c, p, loc) :: l ->
        print_string p;
        searched l
    | [] -> failwith "Invalid! Please check your input format."
  in
  searched pos ^ " " ^ string_of_bool st.win
