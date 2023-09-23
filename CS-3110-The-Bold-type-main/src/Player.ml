type color =
  | Red
  | Green
  | Blue
  | Yellow

type t = {
  color : color;
  pieces : (color * string * int) list;
}

let make c = { color = c; pieces = [] }

type piece_num =
  | One
  | Two
  | Three
  | Four

let home player =
  match player.color with
  | Red ->
      [
        (Red, "red 1", 0);
        (Red, "red 2", 0);
        (Red, "red 3", 0);
        (Red, "red 4", 0);
      ]
  | Green ->
      [
        (Green, "green 1", 0);
        (Green, "green 2", 0);
        (Green, "green 3", 0);
        (Green, "green 4", 0);
      ]
  | Blue ->
      [
        (Blue, "blue 1", 0);
        (Blue, "blue 2", 0);
        (Blue, "blue 3", 0);
        (Blue, "blue 4", 0);
      ]
  | Yellow ->
      [
        (Yellow, "yellow 1", 0);
        (Yellow, "yellow 2", 0);
        (Yellow, "yellow 3", 0);
        (Yellow, "yellow 4", 0);
      ]

let over player =
  match player.color with
  | Red ->
      [
        (Red, "red 1", 56);
        (Red, "red 2", 56);
        (Red, "red 3", 56);
        (Red, "red 4", 56);
      ]
  | Green ->
      [
        (Green, "green 1", 56);
        (Green, "green 2", 56);
        (Green, "green 3", 56);
        (Green, "green 4", 56);
      ]
  | Blue ->
      [
        (Blue, "blue 1", 56);
        (Blue, "blue 2", 56);
        (Blue, "blue 3", 56);
        (Blue, "blue 4", 56);
      ]
  | Yellow ->
      [
        (Yellow, "yellow 1", 56);
        (Yellow, "yellow 2", 56);
        (Yellow, "yellow 3", 56);
        (Yellow, "yellow 4", 56);
      ]

let get_color player = player.color
