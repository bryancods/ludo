open OUnit2
open Game
open Command
open Move
open Player
open State
open Board

let dice_roll_test (name : string) (expected_output : int) : test =
  name >:: fun _ ->
  assert_equal true
    (let r = dice_roll () in
     r >= 0 && r < expected_output)
    ~msg:"Unexpected output"

let make_move_test (name : string) (input1 : State.t) (input2 : phrase)
    (expected_output : (color * phrase * int) list) : test =
  name >:: fun _ -> assert (expected_output != make_move input1 input2)

let init_state_test (name : string) (input : Player.t)
    (expected_output : State.t) : test =
  name >:: fun _ ->
  assert_equal expected_output (init_state input) ~msg:"Unexpected output"

let end_pos_test (name : string) (input : State.t) (expected_output : bool) :
    test =
  name >:: fun _ ->
  assert_equal expected_output input.win ~msg:"Unexpected output"

let state_test =
  let red =
    {
      color = Red;
      pieces =
        [
          (Red, "red 1", 0);
          (Red, "red 2", 0);
          (Red, "red 3", 0);
          (Red, "red 4", 0);
        ];
    }
  in
  let green =
    {
      color = Green;
      pieces =
        [
          (Green, "green 1", 0);
          (Green, "green 2", 0);
          (Green, "green 3", 0);
          (Green, "green 4", 0);
        ];
    }
  in
  let blue =
    {
      color = Blue;
      pieces =
        [
          (Blue, "blue 1", 0);
          (Blue, "blue 2", 0);
          (Blue, "blue 3", 0);
          (Blue, "blue 4", 0);
        ];
    }
  in
  let yellow =
    {
      color = Yellow;
      pieces =
        [
          (Yellow, "yellow 1", 0);
          (Yellow, "yellow 2", 0);
          (Yellow, "yellow 3", 0);
          (Yellow, "yellow 4", 0);
        ];
    }
  in

  [
    dice_roll_test "Testing dice_roll is valid" 6;
    make_move_test "move from init_board yellow"
      {
        player = yellow;
        position =
          [
            (Yellow, "yellow 1", 0);
            (Yellow, "yellow 2", 0);
            (Yellow, "yellow 3", 0);
            (Yellow, "yellow 4", 0);
          ];
        win = false;
      }
      "yellow 2"
      [
        (Yellow, "yellow 1", 0);
        (Yellow, "yellow 2", 0);
        (Yellow, "yellow 3", 0);
        (Yellow, "yellow 4", 0);
      ];
    make_move_test "move from init_board red"
      {
        player = red;
        position =
          [
            (Red, "red 1", 0);
            (Red, "red 2", 0);
            (Red, "red 3", 0);
            (Red, "red 4", 0);
          ];
        win = false;
      }
      "red 2"
      [
        (Red, "red 1", 0);
        (Red, "red 2", 0);
        (Red, "red 3", 0);
        (Red, "red 4", 0);
      ];
    make_move_test "move from init_board green"
      {
        player = green;
        position =
          [
            (Green, "green 1", 0);
            (Green, "green 2", 0);
            (Green, "green 3", 0);
            (Green, "green 4", 0);
          ];
        win = false;
      }
      "green 2"
      [
        (Green, "green 1", 0);
        (Green, "green 2", 0);
        (Green, "green 3", 0);
        (Green, "green 4", 0);
      ];
    make_move_test "move from init_board blue"
      {
        player = blue;
        position =
          [
            (Blue, "blue 1", 0);
            (Blue, "blue 2", 0);
            (Blue, "blue 3", 0);
            (Blue, "blue 4", 0);
          ];
        win = false;
      }
      "blue 2"
      [
        (Blue, "blue 1", 0);
        (Blue, "blue 2", 0);
        (Blue, "blue 3", 0);
        (Blue, "blue 4", 0);
      ];
    make_move_test "move during game yellow"
      {
        player = yellow;
        position =
          [
            (Yellow, "yellow 1", 15);
            (Yellow, "yellow 2", 10);
            (Yellow, "yellow 3", 34);
            (Yellow, "yellow 4", 19);
          ];
        win = false;
      }
      "yellow 4"
      [
        (Yellow, "yellow 1", 15);
        (Yellow, "yellow 2", 10);
        (Yellow, "yellow 3", 34);
        (Yellow, "yellow 4", 19);
      ];
    make_move_test "move during game red"
      {
        player = red;
        position =
          [
            (Red, "red 1", 11);
            (Red, "red 2", 23);
            (Red, "red 3", 17);
            (Red, "red 4", 18);
          ];
        win = false;
      }
      "red 4"
      [
        (Red, "red 1", 11);
        (Red, "red 2", 23);
        (Red, "red 3", 17);
        (Red, "red 4", 18);
      ];
    make_move_test "move during game green"
      {
        player = green;
        position =
          [
            (Green, "green 1", 7);
            (Green, "green 2", 15);
            (Green, "green 3", 30);
            (Green, "green 4", 24);
          ];
        win = false;
      }
      "green 4"
      [
        (Green, "green 1", 7);
        (Green, "green 2", 15);
        (Green, "green 3", 30);
        (Green, "green 4", 24);
      ];
    make_move_test "move during game blue"
      {
        player = blue;
        position =
          [
            (Blue, "blue 1", 16);
            (Blue, "blue 2", 26);
            (Blue, "blue 3", 36);
            (Blue, "blue 4", 33);
          ];
        win = false;
      }
      "blue 4"
      [
        (Blue, "blue 1", 16);
        (Blue, "blue 2", 26);
        (Blue, "blue 3", 36);
        (Blue, "blue 4", 33);
      ];
    init_state_test "Testing initial state of red piece" red
      {
        player = red;
        position =
          [
            (Red, "red 1", 0);
            (Red, "red 2", 0);
            (Red, "red 3", 0);
            (Red, "red 4", 0);
          ];
        win = false;
      };
    init_state_test "Testing initial state of green piece" green
      {
        player = green;
        position =
          [
            (Green, "green 1", 0);
            (Green, "green 2", 0);
            (Green, "green 3", 0);
            (Green, "green 4", 0);
          ];
        win = false;
      };
    init_state_test "Testing initial state of blue piece" blue
      {
        player = blue;
        position =
          [
            (Blue, "blue 1", 0);
            (Blue, "blue 2", 0);
            (Blue, "blue 3", 0);
            (Blue, "blue 4", 0);
          ];
        win = false;
      };
    init_state_test "Testing initial state of yellow piece" yellow
      {
        player = yellow;
        position =
          [
            (Yellow, "yellow 1", 0);
            (Yellow, "yellow 2", 0);
            (Yellow, "yellow 3", 0);
            (Yellow, "yellow 4", 0);
          ];
        win = false;
      };
    end_pos_test "Testing end state of red piece (false)"
      {
        player = red;
        position =
          [
            (Red, "red 1", 0);
            (Red, "red 2", 0);
            (Red, "red 3", 0);
            (Red, "red 4", 0);
          ];
        win = false;
      }
      false;
    end_pos_test "Testing end state of green piece (false)"
      {
        player = green;
        position =
          [
            (Green, "green 1", 0);
            (Green, "green 2", 0);
            (Green, "green 3", 0);
            (Green, "green 4", 0);
          ];
        win = false;
      }
      false;
    end_pos_test "Testing end state of blue piece (false)"
      {
        player = blue;
        position =
          [
            (Blue, "blue 1", 0);
            (Blue, "blue 2", 0);
            (Blue, "blue 3", 0);
            (Blue, "blue 4", 0);
          ];
        win = false;
      }
      false;
    end_pos_test "Testing end state of yellow piece (false)"
      {
        player = yellow;
        position =
          [
            (Yellow, "yellow 1", 0);
            (Yellow, "yellow 2", 0);
            (Yellow, "yellow 3", 0);
            (Yellow, "yellow 4", 0);
          ];
        win = false;
      }
      false;
    end_pos_test "Testing end state of red piece (true)"
      {
        player = red;
        position =
          [
            (Red, "red 1", 56);
            (Red, "red 2", 56);
            (Red, "red 3", 56);
            (Red, "red 4", 56);
          ];
        win = true;
      }
      true;
    end_pos_test "Testing end state of green piece (true)"
      {
        player = green;
        position =
          [
            (Green, "green 1", 56);
            (Green, "green 2", 56);
            (Green, "green 3", 56);
            (Green, "green 4", 56);
          ];
        win = true;
      }
      true;
    end_pos_test "Testing end state of blue piece (true)"
      {
        player = blue;
        position =
          [
            (Blue, "blue 1", 56);
            (Blue, "blue 2", 56);
            (Blue, "blue 3", 56);
            (Blue, "blue 4", 56);
          ];
        win = true;
      }
      true;
    end_pos_test "Testing end state of yellow piece (true)"
      {
        player = yellow;
        position =
          [
            (Yellow, "yellow 1", 56);
            (Yellow, "yellow 2", 56);
            (Yellow, "yellow 3", 56);
            (Yellow, "yellow 4", 56);
          ];
        win = true;
      }
      true;
  ]

let rec string_of_pieces (p : (color * string * int) list) =
  match p with
  | (col, s, i) :: t ->
      string_of_color col ^ " " ^ s ^ " " ^ string_of_int i ^ string_of_pieces t
  | _ -> ""

let string_of_player (player : Player.t) =
  match player with
  | { color; pieces } ->
      "player: " ^ string_of_color color ^ " pieces: " ^ string_of_pieces pieces

let home_test (name : string) (input : Player.t)
    (expected_output : (color * string * int) list) : test =
  name >:: fun _ ->
  assert_equal expected_output (home input) ~msg:"Unexpected output"

let get_color_test (name : string) (input : Player.t) (expected_output : color)
    : test =
  name >:: fun _ ->
  assert_equal expected_output (get_color input) ~msg:"Unexpected output"

let over_test (name : string) (input : Player.t)
    (expected_output : (color * string * int) list) : test =
  name >:: fun _ ->
  assert_equal expected_output (over input) ~msg:"Unexpected output"

let player_test =
  let red = { color = Red; pieces = [] } in
  let green = { color = Green; pieces = [] } in
  let blue = { color = Blue; pieces = [] } in
  let yellow = { color = Yellow; pieces = [] } in
  [
    home_test "Red player home" red
      [
        (Red, "red 1", 0);
        (Red, "red 2", 0);
        (Red, "red 3", 0);
        (Red, "red 4", 0);
      ];
    home_test "Green player home" green
      [
        (Green, "green 1", 0);
        (Green, "green 2", 0);
        (Green, "green 3", 0);
        (Green, "green 4", 0);
      ];
    home_test "Blue player home" blue
      [
        (Blue, "blue 1", 0);
        (Blue, "blue 2", 0);
        (Blue, "blue 3", 0);
        (Blue, "blue 4", 0);
      ];
    home_test "Yellow player home" yellow
      [
        (Yellow, "yellow 1", 0);
        (Yellow, "yellow 2", 0);
        (Yellow, "yellow 3", 0);
        (Yellow, "yellow 4", 0);
      ];
    over_test "Red player over" red
      [
        (Red, "red 1", 56);
        (Red, "red 2", 56);
        (Red, "red 3", 56);
        (Red, "red 4", 56);
      ];
    over_test "Green player over" green
      [
        (Green, "green 1", 56);
        (Green, "green 2", 56);
        (Green, "green 3", 56);
        (Green, "green 4", 56);
      ];
    over_test "Blue player over" blue
      [
        (Blue, "blue 1", 56);
        (Blue, "blue 2", 56);
        (Blue, "blue 3", 56);
        (Blue, "blue 4", 56);
      ];
    over_test "Yellow player over" yellow
      [
        (Yellow, "yellow 1", 56);
        (Yellow, "yellow 2", 56);
        (Yellow, "yellow 3", 56);
        (Yellow, "yellow 4", 56);
      ];
    get_color_test "get_color: red" red Red;
    get_color_test "get_color: green" green Green;
    get_color_test "get_color: blue" blue Blue;
    get_color_test "get_color: yellow" yellow Yellow;
  ]

let string_of_piece_test (name : string) (input : piece_num)
    (expected_output : string) : test =
  name >:: fun _ ->
  assert_equal expected_output (string_of_piece input) ~msg:"Unexpected output"

let string_of_color_test (name : string) (input : Player.color)
    (expected_output : string) : test =
  name >:: fun _ ->
  assert_equal expected_output (string_of_color input) ~msg:"Unexpected output"

let make_turn_test (name : string) (input : board_state * Command.phrase)
    (expected_output : board_state) : test =
  name >:: fun _ ->
  assert (expected_output != make_turn (fst input) (snd input))

let print_board_test (name : string) (input : t) (expected_output : string) :
    test =
  name >:: fun _ ->
  assert_equal expected_output (print_board input) ~msg:(print_board input)

let send_back_test (name : string) (input : board_state)
    (expected_output : board_state) : test =
  name >:: fun _ ->
  assert_equal expected_output input ~msg:(print_board input.pieces)

let equal_test (name : string) (input : bool) (expected_output : bool) : test =
  name >:: fun _ ->
  assert_equal expected_output input ~msg:(string_of_bool input)

let board_test =
  [
    string_of_color_test "Red" Red "red";
    string_of_color_test "Green" Green "green";
    string_of_color_test "Blue" Blue "blue";
    string_of_color_test "Yellow" Yellow "yellow";
    string_of_piece_test "One" One "1";
    string_of_piece_test "Two" Two "2";
    string_of_piece_test "Three" Three "3";
    string_of_piece_test "Four" Four "4";
    make_turn_test "First turn, Red player" (start_board, "red 2")
      {
        pieces =
          [
            { player = Red; piece = One; location = 0 };
            { player = Red; piece = Two; location = 0 };
            { player = Red; piece = Three; location = 0 };
            { player = Red; piece = Four; location = 0 };
            { player = Green; piece = One; location = 0 };
            { player = Green; piece = Two; location = 0 };
            { player = Green; piece = Three; location = 0 };
            { player = Green; piece = Four; location = 0 };
            { player = Blue; piece = One; location = 0 };
            { player = Blue; piece = Two; location = 0 };
            { player = Blue; piece = Three; location = 0 };
            { player = Blue; piece = Four; location = 0 };
            { player = Yellow; piece = One; location = 0 };
            { player = Yellow; piece = Two; location = 0 };
            { player = Yellow; piece = Three; location = 0 };
            { player = Yellow; piece = Four; location = 0 };
          ];
        player_turn = Green;
        game_status = false;
      };
    make_turn_test "First turn, Red player" (start_board, "red 1")
      {
        pieces =
          [
            { player = Red; piece = One; location = 0 };
            { player = Red; piece = Two; location = 0 };
            { player = Red; piece = Three; location = 0 };
            { player = Red; piece = Four; location = 0 };
            { player = Green; piece = One; location = 0 };
            { player = Green; piece = Two; location = 0 };
            { player = Green; piece = Three; location = 0 };
            { player = Green; piece = Four; location = 0 };
            { player = Blue; piece = One; location = 0 };
            { player = Blue; piece = Two; location = 0 };
            { player = Blue; piece = Three; location = 0 };
            { player = Blue; piece = Four; location = 0 };
            { player = Yellow; piece = One; location = 0 };
            { player = Yellow; piece = Two; location = 0 };
            { player = Yellow; piece = Three; location = 0 };
            { player = Yellow; piece = Four; location = 0 };
          ];
        player_turn = Green;
        game_status = false;
      };
    make_turn_test "First turn, Red player" (start_board, "red 3")
      {
        pieces =
          [
            { player = Red; piece = One; location = 0 };
            { player = Red; piece = Two; location = 0 };
            { player = Red; piece = Three; location = 0 };
            { player = Red; piece = Four; location = 0 };
            { player = Green; piece = One; location = 0 };
            { player = Green; piece = Two; location = 0 };
            { player = Green; piece = Three; location = 0 };
            { player = Green; piece = Four; location = 0 };
            { player = Blue; piece = One; location = 0 };
            { player = Blue; piece = Two; location = 0 };
            { player = Blue; piece = Three; location = 0 };
            { player = Blue; piece = Four; location = 0 };
            { player = Yellow; piece = One; location = 0 };
            { player = Yellow; piece = Two; location = 0 };
            { player = Yellow; piece = Three; location = 0 };
            { player = Yellow; piece = Four; location = 0 };
          ];
        player_turn = Green;
        game_status = false;
      };
    make_turn_test "First turn, Red player" (start_board, "red 4")
      {
        pieces =
          [
            { player = Red; piece = One; location = 0 };
            { player = Red; piece = Two; location = 0 };
            { player = Red; piece = Three; location = 0 };
            { player = Red; piece = Four; location = 0 };
            { player = Green; piece = One; location = 0 };
            { player = Green; piece = Two; location = 0 };
            { player = Green; piece = Three; location = 0 };
            { player = Green; piece = Four; location = 0 };
            { player = Blue; piece = One; location = 0 };
            { player = Blue; piece = Two; location = 0 };
            { player = Blue; piece = Three; location = 0 };
            { player = Blue; piece = Four; location = 0 };
            { player = Yellow; piece = One; location = 0 };
            { player = Yellow; piece = Two; location = 0 };
            { player = Yellow; piece = Three; location = 0 };
            { player = Yellow; piece = Four; location = 0 };
          ];
        player_turn = Green;
        game_status = false;
      };
    make_turn_test "First turn, Blue player"
      ( { pieces = init_board; player_turn = Blue; game_status = false },
        "blue 1" )
      {
        pieces =
          [
            { player = Red; piece = One; location = 0 };
            { player = Red; piece = Two; location = 0 };
            { player = Red; piece = Three; location = 0 };
            { player = Red; piece = Four; location = 0 };
            { player = Green; piece = One; location = 0 };
            { player = Green; piece = Two; location = 0 };
            { player = Green; piece = Three; location = 0 };
            { player = Green; piece = Four; location = 0 };
            { player = Blue; piece = One; location = 0 };
            { player = Blue; piece = Two; location = 0 };
            { player = Blue; piece = Three; location = 0 };
            { player = Blue; piece = Four; location = 0 };
            { player = Yellow; piece = One; location = 0 };
            { player = Yellow; piece = Two; location = 0 };
            { player = Yellow; piece = Three; location = 0 };
            { player = Yellow; piece = Four; location = 0 };
          ];
        player_turn = Yellow;
        game_status = false;
      };
    make_turn_test "First turn, Blue player"
      ( { pieces = init_board; player_turn = Blue; game_status = false },
        "blue 2" )
      {
        pieces =
          [
            { player = Red; piece = One; location = 0 };
            { player = Red; piece = Two; location = 0 };
            { player = Red; piece = Three; location = 0 };
            { player = Red; piece = Four; location = 0 };
            { player = Green; piece = One; location = 0 };
            { player = Green; piece = Two; location = 0 };
            { player = Green; piece = Three; location = 0 };
            { player = Green; piece = Four; location = 0 };
            { player = Blue; piece = One; location = 0 };
            { player = Blue; piece = Two; location = 0 };
            { player = Blue; piece = Three; location = 0 };
            { player = Blue; piece = Four; location = 0 };
            { player = Yellow; piece = One; location = 0 };
            { player = Yellow; piece = Two; location = 0 };
            { player = Yellow; piece = Three; location = 0 };
            { player = Yellow; piece = Four; location = 0 };
          ];
        player_turn = Yellow;
        game_status = false;
      };
    make_turn_test "First turn, Blue player"
      ( { pieces = init_board; player_turn = Blue; game_status = false },
        "blue 3" )
      {
        pieces =
          [
            { player = Red; piece = One; location = 0 };
            { player = Red; piece = Two; location = 0 };
            { player = Red; piece = Three; location = 0 };
            { player = Red; piece = Four; location = 0 };
            { player = Green; piece = One; location = 0 };
            { player = Green; piece = Two; location = 0 };
            { player = Green; piece = Three; location = 0 };
            { player = Green; piece = Four; location = 0 };
            { player = Blue; piece = One; location = 0 };
            { player = Blue; piece = Two; location = 0 };
            { player = Blue; piece = Three; location = 0 };
            { player = Blue; piece = Four; location = 0 };
            { player = Yellow; piece = One; location = 0 };
            { player = Yellow; piece = Two; location = 0 };
            { player = Yellow; piece = Three; location = 0 };
            { player = Yellow; piece = Four; location = 0 };
          ];
        player_turn = Yellow;
        game_status = false;
      };
    make_turn_test "First turn, Blue player"
      ( { pieces = init_board; player_turn = Blue; game_status = false },
        "blue 4" )
      {
        pieces =
          [
            { player = Red; piece = One; location = 0 };
            { player = Red; piece = Two; location = 0 };
            { player = Red; piece = Three; location = 0 };
            { player = Red; piece = Four; location = 0 };
            { player = Green; piece = One; location = 0 };
            { player = Green; piece = Two; location = 0 };
            { player = Green; piece = Three; location = 0 };
            { player = Green; piece = Four; location = 0 };
            { player = Blue; piece = One; location = 0 };
            { player = Blue; piece = Two; location = 0 };
            { player = Blue; piece = Three; location = 0 };
            { player = Blue; piece = Four; location = 0 };
            { player = Yellow; piece = One; location = 0 };
            { player = Yellow; piece = Two; location = 0 };
            { player = Yellow; piece = Three; location = 0 };
            { player = Yellow; piece = Four; location = 0 };
          ];
        player_turn = Yellow;
        game_status = false;
      };
    make_turn_test "First turn, Green player"
      ( { pieces = init_board; player_turn = Green; game_status = false },
        "green 1" )
      {
        pieces =
          [
            { player = Red; piece = One; location = 0 };
            { player = Red; piece = Two; location = 0 };
            { player = Red; piece = Three; location = 0 };
            { player = Red; piece = Four; location = 0 };
            { player = Green; piece = One; location = 0 };
            { player = Green; piece = Two; location = 0 };
            { player = Green; piece = Three; location = 0 };
            { player = Green; piece = Four; location = 0 };
            { player = Blue; piece = One; location = 0 };
            { player = Blue; piece = Two; location = 0 };
            { player = Blue; piece = Three; location = 0 };
            { player = Blue; piece = Four; location = 0 };
            { player = Yellow; piece = One; location = 0 };
            { player = Yellow; piece = Two; location = 0 };
            { player = Yellow; piece = Three; location = 0 };
            { player = Yellow; piece = Four; location = 0 };
          ];
        player_turn = Blue;
        game_status = false;
      };
    make_turn_test "First turn, Green player"
      ( { pieces = init_board; player_turn = Green; game_status = false },
        "green 2" )
      {
        pieces =
          [
            { player = Red; piece = One; location = 0 };
            { player = Red; piece = Two; location = 0 };
            { player = Red; piece = Three; location = 0 };
            { player = Red; piece = Four; location = 0 };
            { player = Green; piece = One; location = 0 };
            { player = Green; piece = Two; location = 0 };
            { player = Green; piece = Three; location = 0 };
            { player = Green; piece = Four; location = 0 };
            { player = Blue; piece = One; location = 0 };
            { player = Blue; piece = Two; location = 0 };
            { player = Blue; piece = Three; location = 0 };
            { player = Blue; piece = Four; location = 0 };
            { player = Yellow; piece = One; location = 0 };
            { player = Yellow; piece = Two; location = 0 };
            { player = Yellow; piece = Three; location = 0 };
            { player = Yellow; piece = Four; location = 0 };
          ];
        player_turn = Blue;
        game_status = false;
      };
    make_turn_test "First turn, Green player"
      ( { pieces = init_board; player_turn = Green; game_status = false },
        "green 3" )
      {
        pieces =
          [
            { player = Red; piece = One; location = 0 };
            { player = Red; piece = Two; location = 0 };
            { player = Red; piece = Three; location = 0 };
            { player = Red; piece = Four; location = 0 };
            { player = Green; piece = One; location = 0 };
            { player = Green; piece = Two; location = 0 };
            { player = Green; piece = Three; location = 0 };
            { player = Green; piece = Four; location = 0 };
            { player = Blue; piece = One; location = 0 };
            { player = Blue; piece = Two; location = 0 };
            { player = Blue; piece = Three; location = 0 };
            { player = Blue; piece = Four; location = 0 };
            { player = Yellow; piece = One; location = 0 };
            { player = Yellow; piece = Two; location = 0 };
            { player = Yellow; piece = Three; location = 0 };
            { player = Yellow; piece = Four; location = 0 };
          ];
        player_turn = Blue;
        game_status = false;
      };
    make_turn_test "First turn, Green player"
      ( { pieces = init_board; player_turn = Green; game_status = false },
        "green 4" )
      {
        pieces =
          [
            { player = Red; piece = One; location = 0 };
            { player = Red; piece = Two; location = 0 };
            { player = Red; piece = Three; location = 0 };
            { player = Red; piece = Four; location = 0 };
            { player = Green; piece = One; location = 0 };
            { player = Green; piece = Two; location = 0 };
            { player = Green; piece = Three; location = 0 };
            { player = Green; piece = Four; location = 0 };
            { player = Blue; piece = One; location = 0 };
            { player = Blue; piece = Two; location = 0 };
            { player = Blue; piece = Three; location = 0 };
            { player = Blue; piece = Four; location = 0 };
            { player = Yellow; piece = One; location = 0 };
            { player = Yellow; piece = Two; location = 0 };
            { player = Yellow; piece = Three; location = 0 };
            { player = Yellow; piece = Four; location = 0 };
          ];
        player_turn = Blue;
        game_status = false;
      };
    make_turn_test "First turn, Yellow player"
      ( { pieces = init_board; player_turn = Yellow; game_status = false },
        "yellow 1" )
      {
        pieces =
          [
            { player = Red; piece = One; location = 0 };
            { player = Red; piece = Two; location = 0 };
            { player = Red; piece = Three; location = 0 };
            { player = Red; piece = Four; location = 0 };
            { player = Green; piece = One; location = 0 };
            { player = Green; piece = Two; location = 0 };
            { player = Green; piece = Three; location = 0 };
            { player = Green; piece = Four; location = 0 };
            { player = Blue; piece = One; location = 0 };
            { player = Blue; piece = Two; location = 0 };
            { player = Blue; piece = Three; location = 0 };
            { player = Blue; piece = Four; location = 0 };
            { player = Yellow; piece = One; location = 0 };
            { player = Yellow; piece = Two; location = 0 };
            { player = Yellow; piece = Three; location = 0 };
            { player = Yellow; piece = Four; location = 0 };
          ];
        player_turn = Red;
        game_status = false;
      };
    make_turn_test "First turn, Yellow player"
      ( { pieces = init_board; player_turn = Yellow; game_status = false },
        "yellow 2" )
      {
        pieces =
          [
            { player = Red; piece = One; location = 0 };
            { player = Red; piece = Two; location = 0 };
            { player = Red; piece = Three; location = 0 };
            { player = Red; piece = Four; location = 0 };
            { player = Green; piece = One; location = 0 };
            { player = Green; piece = Two; location = 0 };
            { player = Green; piece = Three; location = 0 };
            { player = Green; piece = Four; location = 0 };
            { player = Blue; piece = One; location = 0 };
            { player = Blue; piece = Two; location = 0 };
            { player = Blue; piece = Three; location = 0 };
            { player = Blue; piece = Four; location = 0 };
            { player = Yellow; piece = One; location = 0 };
            { player = Yellow; piece = Two; location = 0 };
            { player = Yellow; piece = Three; location = 0 };
            { player = Yellow; piece = Four; location = 0 };
          ];
        player_turn = Red;
        game_status = false;
      };
    make_turn_test "First turn, Yellow player"
      ( { pieces = init_board; player_turn = Yellow; game_status = false },
        "yellow 3" )
      {
        pieces =
          [
            { player = Red; piece = One; location = 0 };
            { player = Red; piece = Two; location = 0 };
            { player = Red; piece = Three; location = 0 };
            { player = Red; piece = Four; location = 0 };
            { player = Green; piece = One; location = 0 };
            { player = Green; piece = Two; location = 0 };
            { player = Green; piece = Three; location = 0 };
            { player = Green; piece = Four; location = 0 };
            { player = Blue; piece = One; location = 0 };
            { player = Blue; piece = Two; location = 0 };
            { player = Blue; piece = Three; location = 0 };
            { player = Blue; piece = Four; location = 0 };
            { player = Yellow; piece = One; location = 0 };
            { player = Yellow; piece = Two; location = 0 };
            { player = Yellow; piece = Three; location = 0 };
            { player = Yellow; piece = Four; location = 0 };
          ];
        player_turn = Red;
        game_status = false;
      };
    make_turn_test "First turn, Yellow player"
      ( { pieces = init_board; player_turn = Yellow; game_status = false },
        "yellow 4" )
      {
        pieces =
          [
            { player = Red; piece = One; location = 0 };
            { player = Red; piece = Two; location = 0 };
            { player = Red; piece = Three; location = 0 };
            { player = Red; piece = Four; location = 0 };
            { player = Green; piece = One; location = 0 };
            { player = Green; piece = Two; location = 0 };
            { player = Green; piece = Three; location = 0 };
            { player = Green; piece = Four; location = 0 };
            { player = Blue; piece = One; location = 0 };
            { player = Blue; piece = Two; location = 0 };
            { player = Blue; piece = Three; location = 0 };
            { player = Blue; piece = Four; location = 0 };
            { player = Yellow; piece = One; location = 0 };
            { player = Yellow; piece = Two; location = 0 };
            { player = Yellow; piece = Three; location = 0 };
            { player = Yellow; piece = Four; location = 0 };
          ];
        player_turn = Red;
        game_status = false;
      };
    print_board_test "Empty board" [] "";
    send_back_test "one send back"
      (send_back
         {
           pieces =
             [
               { player = Red; piece = One; location = 34 };
               { player = Red; piece = Two; location = 0 };
               { player = Red; piece = Three; location = 0 };
               { player = Red; piece = Four; location = 0 };
               { player = Green; piece = One; location = 34 };
               { player = Green; piece = Two; location = 0 };
               { player = Green; piece = Three; location = 0 };
               { player = Green; piece = Four; location = 0 };
               { player = Blue; piece = One; location = 0 };
               { player = Blue; piece = Two; location = 0 };
               { player = Blue; piece = Three; location = 0 };
               { player = Blue; piece = Four; location = 0 };
               { player = Yellow; piece = One; location = 0 };
               { player = Yellow; piece = Two; location = 0 };
               { player = Yellow; piece = Three; location = 0 };
               { player = Yellow; piece = Four; location = 0 };
             ];
           player_turn = Red;
           game_status = false;
         })
      {
        pieces =
          [
            { player = Red; piece = One; location = 34 };
            { player = Red; piece = Two; location = 0 };
            { player = Red; piece = Three; location = 0 };
            { player = Red; piece = Four; location = 0 };
            { player = Green; piece = One; location = 0 };
            { player = Green; piece = Two; location = 0 };
            { player = Green; piece = Three; location = 0 };
            { player = Green; piece = Four; location = 0 };
            { player = Blue; piece = One; location = 0 };
            { player = Blue; piece = Two; location = 0 };
            { player = Blue; piece = Three; location = 0 };
            { player = Blue; piece = Four; location = 0 };
            { player = Yellow; piece = One; location = 0 };
            { player = Yellow; piece = Two; location = 0 };
            { player = Yellow; piece = Three; location = 0 };
            { player = Yellow; piece = Four; location = 0 };
          ];
        player_turn = Red;
        game_status = false;
      };
    send_back_test "nothing to send back"
      (send_back
         {
           pieces =
             [
               { player = Red; piece = One; location = 1 };
               { player = Red; piece = Two; location = 2 };
               { player = Red; piece = Three; location = 3 };
               { player = Red; piece = Four; location = 4 };
               { player = Green; piece = One; location = 5 };
               { player = Green; piece = Two; location = 6 };
               { player = Green; piece = Three; location = 10 };
               { player = Green; piece = Four; location = 11 };
               { player = Blue; piece = One; location = 12 };
               { player = Blue; piece = Two; location = 13 };
               { player = Blue; piece = Three; location = 14 };
               { player = Blue; piece = Four; location = 15 };
               { player = Yellow; piece = One; location = 16 };
               { player = Yellow; piece = Two; location = 17 };
               { player = Yellow; piece = Three; location = 18 };
               { player = Yellow; piece = Four; location = 19 };
             ];
           player_turn = Red;
           game_status = false;
         })
      {
        pieces =
          [
            { player = Red; piece = One; location = 1 };
            { player = Red; piece = Two; location = 2 };
            { player = Red; piece = Three; location = 3 };
            { player = Red; piece = Four; location = 4 };
            { player = Green; piece = One; location = 5 };
            { player = Green; piece = Two; location = 6 };
            { player = Green; piece = Three; location = 10 };
            { player = Green; piece = Four; location = 11 };
            { player = Blue; piece = One; location = 12 };
            { player = Blue; piece = Two; location = 13 };
            { player = Blue; piece = Three; location = 14 };
            { player = Blue; piece = Four; location = 15 };
            { player = Yellow; piece = One; location = 16 };
            { player = Yellow; piece = Two; location = 17 };
            { player = Yellow; piece = Three; location = 18 };
            { player = Yellow; piece = Four; location = 19 };
          ];
        player_turn = Red;
        game_status = false;
      };
  ]

let parse_instructions_test (name : string) (input : string)
    (expected_output : other_command option) : test =
  name >:: fun _ ->
  assert_equal expected_output (parse_instructions input)
    ~msg:"Unexpected output"

let parse_piece_test (name : string) (input : string)
    (expected_output : piece_num) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (Command.parse_piece input)
    ~msg:"Unexpected output"

let parse_player_test (name : string) (input : string)
    (expected_output : Player.color) : test =
  name >:: fun _ ->
  assert_equal expected_output (parse_player input) ~msg:"Unexpected output"

let parse_command_test (name : string) (input : string)
    (expected_output : command) : test =
  name >:: fun _ ->
  assert_equal expected_output (parse_command input) ~msg:"Unexpected output"

let command_test =
  [
    parse_instructions_test "parse_instructions with 'yes'" "yes" (Some Yes);
    parse_instructions_test "parse_instructions with 'no'" "no" (Some No);
    parse_instructions_test "parse_instructions with empty string" "" None;
    parse_instructions_test "parse_instructions with malformed input" "foo" None;
    parse_player_test "parse_player with 'red'" "red" Red;
    parse_player_test "parse_player with 'green'" "green" Green;
    parse_player_test "parse_player\n   with 'blue'" "blue" Blue;
    parse_player_test "parse_player with 'yellow'" "yellow" Yellow;
    ( "parse_player with invalid input" >:: fun _ ->
      assert_raises Invalid (fun () -> parse_player "foo") );
    parse_piece_test "parse_piece with 1" "1" One;
    parse_piece_test "parse_piece with 2" "2" Two;
    parse_piece_test "parse_piece with 3" "3" Three;
    parse_piece_test "parse_piece with 4" "4" Four;
    ( "parse_piece with invalid input" >:: fun _ ->
      assert_raises Invalid (fun () -> parse_piece "foo") );
    parse_command_test "parse_command with\n 'quit'" "quit" Quit;
    parse_command_test "parse_command\n with 'q'" "q" Quit;
    parse_command_test "parse_command with 'move piece 1'" "move red 1"
      (Move "red 1");
    parse_command_test "parse_command with 'm\n piece\n 2'" "m blue 2"
      (Move "blue 2");
    parse_command_test "parse_command\n\n\n with\n empty string" ""
      (Invalid "Invalid Command");
    parse_command_test "parse_command with malformed input" "foo"
      (Invalid "Invalid Command");
  ]

let get_player_test (name : string) (input : Move.t)
    (expected_output : Player.color) : test =
  name >:: fun _ ->
  assert_equal expected_output (get_player input) ~msg:"Unexpected output"

let get_piece_test (name : string) (input : Move.t)
    (expected_output : Player.piece_num) : test =
  name >:: fun _ ->
  assert_equal expected_output (get_piece input) ~msg:"Unexpected output"

let get_loc_test (name : string) (input : Move.t) (expected_output : int) : test
    =
  name >:: fun _ ->
  assert_equal expected_output (get_loc input) ~msg:"Unexpected output"

let parse_piece_test (name : string) (input : string)
    (expected_output : Player.piece_num) : test =
  name >:: fun _ -> assert_equal expected_output (parse_piece input) ~msg:input

let parse_player_test (name : string) (input : string)
    (expected_output : Player.color) : test =
  name >:: fun _ ->
  assert_equal expected_output (parse_player input) ~msg:"Unexpected output"

let build_test (name : string) (input : string) (expected_output : Move.t) :
    test =
  name >:: fun _ ->
  assert_equal expected_output (build input) ~msg:"Unexpected output"

let move_test =
  [
    get_player_test "Test get_player 1"
      { player = Red; piece = One; location = 0 }
      Red;
    get_player_test "Test get_player 2"
      { player = Green; piece = Two; location = 0 }
      Green;
    get_player_test "Test get_player 3"
      { player = Blue; piece = Three; location = 0 }
      Blue;
    get_player_test "Test\n\n   get_player 4"
      { player = Yellow; piece = Four; location = 0 }
      Yellow;
    get_piece_test "Test get_piece 1"
      { player = Red; piece = One; location = 0 }
      One;
    get_piece_test "Test get_piece 2"
      { player = Green; piece = Two; location = 0 }
      Two;
    get_piece_test "Test get_piece 3"
      { player = Blue; piece = Three; location = 0 }
      Three;
    get_piece_test "Test get_piece 4"
      { player = Yellow; piece = Four; location = 0 }
      Four;
    get_loc_test "Test\n get_loc 1"
      { player = Red; piece = One; location = 5 }
      5;
    get_loc_test "Test get_loc 2"
      { player = Green; piece = Two; location = 10 }
      10;
    get_loc_test "Test get_loc\n   3"
      { player = Blue; piece = Three; location = 15 }
      15;
    get_loc_test "Test\n   get_loc 4"
      { player = Yellow; piece = Four; location = 20 }
      20;
    parse_piece_test "Test parse_piece 1" "1" One;
    parse_piece_test "Test\n   parse_piece 2" "2" Two;
    parse_piece_test "Test parse_piece 3" "3" Three;
    parse_piece_test "Test parse_piece 4" "4" Four;
    parse_player_test "Test\n   parse_player 1" "red" Red;
    parse_player_test "Test parse_player 2" "green" Green;
    parse_player_test "Test parse_player\n 3" "blue" Blue;
    parse_player_test "Test parse_player 4" "yellow" Yellow;
    build_test "Test\n   build 1" "red 1"
      { player = Red; piece = One; location = 0 };
    build_test "Test build 2" "green 2"
      { player = Green; piece = Two; location = 0 };
    build_test "Test build 3" "blue 3"
      { player = Blue; piece = Three; location = 0 };
    build_test "Test build 4" "yellow 4"
      { player = Yellow; piece = Four; location = 0 };
  ]

let suite =
  "search test suite"
  >::: List.flatten
         [ board_test; state_test; player_test; move_test; command_test ]

let _ = run_test_tt_main suite
