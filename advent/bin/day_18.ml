open Core

let get_direction_v2 = function
  | '0' -> 0, 1
  | '2' -> 0, -1
  | '3' -> -1, 0
  | '1' -> 1, 0
  | _ -> raise (Invalid_argument "invalid direction")
;;

let get_direction = function
  | "R" -> 0, 1
  | "L" -> 0, -1
  | "U" -> -1, 0
  | "D" -> 1, 0
  | _ -> raise (Invalid_argument "invalid direction")
;;

let add_next_point (y, x) dy dx num_steps points =
  let new_point = y + (dy * num_steps), x + (dx * num_steps) in
  points @ [ new_point ]
;;

let parse_point (dy, dx) num_steps points =
  let current_point = List.last_exn points in
  add_next_point current_point dy dx num_steps points
;;

let rec parse_instructions points perimeter_length is_v2 = function
  | [] -> points, perimeter_length
  | instruction_string :: lines ->
    let points, perimeter_length =
      match String.split instruction_string ~on:' ' with
      | [ direction; num_steps; color ] ->
        if is_v2
        then (
          let color =
            String.to_list
              (String.fold color ~init:"" ~f:(fun acc c ->
                 match c with
                 | '(' -> acc
                 | ')' -> acc
                 | '#' -> acc
                 | c -> acc ^ String.of_char c))
          in
          let direction = get_direction_v2 (List.last_exn color) in
          let color = "0x" ^ String.of_char_list (List.drop_last_exn color) in
          let num_steps = Int.of_string color in
          parse_point direction num_steps points, num_steps + perimeter_length)
        else (
          let num_steps = Int.of_string num_steps in
          ( parse_point (get_direction direction) num_steps points
          , perimeter_length + num_steps ))
      | _ -> raise (Invalid_argument "failed parsing")
    in
    parse_instructions points perimeter_length is_v2 lines
;;

let rec shoelace area = function
  | [], _ -> area / 2
  | _, 0 -> area / 2
  | (y, x) :: (yy, xx) :: points, iter ->
    shoelace (area + ((x * yy) - (xx * y))) ([ yy, xx ] @ points @ [ y, x ], iter - 1)
  | _ -> raise (Invalid_argument "")
;;

let lines = Advent.Advent_tools.read_lines "./input/puzzle18.txt"
let points, perimeter_length = parse_instructions [ 0, 0 ] 0 false lines
let len = List.length points
let result = shoelace 0 (points, len) + (perimeter_length / 2) + 1
let () = Fmt.pr "Result: %d\n" result
let points, perimeter_length = parse_instructions [ 0, 0 ] 0 true lines
let len = List.length points
let result = shoelace 0 (points, len) + (perimeter_length / 2) + 1
let () = Fmt.pr "Result: %d\n" result
