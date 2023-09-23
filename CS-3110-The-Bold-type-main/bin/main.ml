open Game
open Command
open Player
open Board
open Move
open State

exception Empty
exception Invalid

let string_of_color (color : color) : string =
  match color with
  | Red -> "red"
  | Green -> "green"
  | Blue -> "blue"
  | Yellow -> "yellow"

let rec instructions () =
  ANSITerminal.print_string [ ANSITerminal.magenta ]
    "Would you like to read the instructions? \n";
  match parse_instructions (read_line ()) with
  | Some Yes ->
      print_endline
        "Ludo is a strategy board game for 4 players. \n\
        \ In this game, your goal is to race your four pieces from your home \
         base to the finish.  \n\
        \ Here are the game rules to get you started:\n\
        \    Each player will start the game with four pieces in their home \
         base. Players will be able to move their pieces based on the number \
         they receive after rolling a dice during their turn. \n\
        \ There are four corners of the board, and each player's has a home \
         base will be in one of those corners depending on their color. \n\
        \ When the game begins you are assigned a color. By rolling the dice, \
         each player will try to get all four of their pieces around the board \
         and into the finish at the center of the board. The player who first \
         gets all four of their pieces to the finish wins the game.\n\
        \  The goal is to make sure to get all your pieces go around the board \
         without getting captured and sent back which delays your progress.\n\
        \  The player who rolls the first six on the dice starts the game. \
         Play then moves in a clockwise direction. If a player rolls a six, \
         they get an extra turn.\n\n\
        \  To start moving a piece, a player must roll a six. Once the piece \
         is in play, the player can move it the number of spaces indicated by \
         the dice roll, following the arrows on the board. Pieces may only \
         move in a clockwise direction. Pieces can land on a space that is \
         already occupied by another player's piece, in which case the \
         occupying piece is captured and given to the piece that captured the \
         previous piece. \n\
        \    For each player's turn:\n\
        \    - To get a piece to leave your home, you must first roll a six. \
         Otherwise, that piece cannot advance onto the board.\n\
        \    - You can choose which piece to move after rolling your die. \
         Depending on what you roll, your chosen piece will move that amount \
         forward on the board. \n\
        \    - If your piece is captured, to get it back to your home you have \
         to roll a six. Then if you want to release it you need to roll \
         another six. \n\
        \    For example, if a Red piece captures Blue piece, the blue piece \
         will be in the Red pieces home base. If the Blue piece wants to get \
         their piece back they will have to roll a six. Then to put in back to \
         the board to continue its adventure, the player needs to roll another \
         six.\n\
        \    Vice versa, if one of your pieces is there when another player \
         gets to that spot, then your piece will have to start over at your \
         home base, and you won't be able to move it until you roll another \
         six.\n\
        \    - If you roll a six and are moving a piece that's outside of your \
         home, then you will get another bonus roll. \n\
        \    - In order to get one of your pieces to the finish, you must roll \
         the exact number needed to land there. \n\
        \    For example, if your piece is 5 spaces away from the finish, you \
         will not advance to the finish with a 6. \n\
        \    - If you are the first person to have all of your pieces reach \
         the finish, you win the game! \n\
        \ Now, let's begin! The Adventure awaits"
  | Some No ->
      ANSITerminal.print_string [ ANSITerminal.magenta ] "Let's begin! \n"
  | None ->
      print_endline "this is an invalid command please type 'yes' or 'no'.";
      instructions ()

let rec number_of_players () =
  ANSITerminal.print_string [ ANSITerminal.magenta ]
    "\n\n How many players are in the game? \n";
  let check = int_of_string (read_line ()) in
  if check > 4 then (
    print_endline "That's too many!";
    number_of_players ())
  else if check < 4 then (
    print_endline "That's not enough!";
    number_of_players ())
  else if check = 4 then
    ANSITerminal.print_string [ ANSITerminal.cyan ]
      "Player 1 your color is Red \n\
       Player 2 your color is Green \n\
       Player 3 your color is Blue \n\
       Player 4 your color is Yellow"
  else number_of_players ();
  let rec confirm () =
    match check with
    | 4 ->
        ANSITerminal.print_string [ ANSITerminal.red ]
          "\n\n Player 1 please type your color. (in lowercase letters only) \n";
        if read_line () <> "red" then (
          print_endline "That's not your color!";
          confirm ())
        else (
          ANSITerminal.print_string [ ANSITerminal.green ]
            "\n\n\
            \ Player 2 please type your color. (in lowercase letters only) \n";
          if read_line () <> "green" then (
            print_endline "That's not your color!";
            confirm ())
          else (
            ANSITerminal.print_string [ ANSITerminal.blue ]
              "\n\n\
              \ Player 3 please type your color. (in lowercase letters only) \n";
            if read_line () <> "blue" then (
              print_endline "That's not your color!";
              confirm ())
            else (
              ANSITerminal.print_string [ ANSITerminal.yellow ]
                "\n\n\
                \ Player 4 please type your color. (in lowercase letters only) \n";
              if read_line () <> "yellow" then (
                print_endline "That's not your color!";
                confirm ()))))
    | _ -> failwith "This is not a valid number of players!"
  in
  confirm ()

(* 1. print out what they rolled*)
let rec go (board : Board.board_state) =
  ANSITerminal.print_string [ ANSITerminal.magenta ]
    ("\n\nIt is " ^ string_of_color board.player_turn ^ "'s turn.\n");
  ANSITerminal.print_string [ ANSITerminal.magenta ]
    ("\n\nWhich piece (1 - 4) do you want to move player "
    ^ string_of_color board.player_turn
    ^ " (response should be of form: move [color] [piece number])? \n");
  let com = read_line () in
  match Command.parse_command com with
  | Quit ->
      ANSITerminal.print_string [ ANSITerminal.magenta ]
        "\n\nYou have quit the game. Thank you for playing !! \n"
  | Move p -> (
      match check_get_player p with
      | None ->
          ANSITerminal.print_string [ ANSITerminal.red ]
            "\n\nThis is an invalid command. Please pick a piece from 1 - 4 \n";
          print_endline "Lets try again";
          go board
      | Some s ->
          if String.equal (string_of_color board.player_turn) s = false then
            go board
          else print_endline (string_of_color board.player_turn);
          print_endline ("You chose to move " ^ p);
          let next_go = make_turn board p in
          print_endline (print_board next_go.pieces);

          let p1 = get_equal board.pieces in
          let p2 = get_equal next_go.pieces in

          if equal p p1 p2 || parse_find_piece (find_moved_piece p1 p2) = 0 then
            ANSITerminal.print_string [ ANSITerminal.Bold ]
              "\n\n No piece got sent back\n"
          else
            ANSITerminal.print_string [ ANSITerminal.red ]
              ("\n\nAfter " ^ p ^ " was moved " ^ find_moved_piece p1 p2
             ^ " got sent back!\n");

          if next_go.game_status = true then
            ANSITerminal.print_string [ ANSITerminal.green ]
              ("\n\nPlayer "
              ^ string_of_color board.player_turn
              ^ " won! GOOD JOB!!\n")
          else go next_go)
  | Invalid p ->
      ANSITerminal.print_string [ ANSITerminal.red ]
        "\n\nThis is an invalid command. Please pick a piece from 1 - 4 \n";
      print_endline "Lets try again";
      go board

let main () =
  ANSITerminal.print_string [ ANSITerminal.magenta ]
    "\n\n LET'S PLAY LUDO :)! \n";
  instructions ();
  number_of_players ();
  go start_board

let () = main ()
