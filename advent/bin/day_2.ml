open Core
open Advent.Day2_util

let rec process_lines_part_1 lines pos =
  match lines with
  | [] -> 0
  | head :: body ->
    let add = if parse_game head then pos + 1 else 0 in
    (*let () = Fmt.pr "line %d %b@." (pos + 1) (add > 0) in*)
    add + process_lines_part_1 body (pos + 1)
;;

let rec process_lines_part_2 lines =
  match lines with
  | [] -> 0
  | head :: body -> parse_minimum_game head + process_lines_part_2 body
;;

let () =
  let lines = Advent.Advent_tools.read_lines "./input/puzzle_2.txt" in
  process_lines_part_1 lines 0 |> Fmt.pr "Result: %d@."
;;

let () =
  let lines = Advent.Advent_tools.read_lines "./input/puzzle_2.txt" in
  process_lines_part_2 lines |> Fmt.pr "Result: %d@."
;;
