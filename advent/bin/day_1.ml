open Core

let () =
  let lines = Advent.Advent_tools.read_lines "./input/puzzle_1.txt" in
  List.fold lines ~init:0 ~f:(fun acc line ->
    let chars = String.to_list line in
    let numbers = List.filter chars ~f:Char.is_digit in
    let number =
      Int.of_string
        (Char.to_string (List.hd_exn numbers) ^ Char.to_string (List.last_exn numbers))
    in
    acc + number)
  |> Fmt.pr "Result: %d@."
;;

let () =
  let lines = Advent.Advent_tools.read_lines "./input/puzzle_1.txt" in
  List.fold lines ~init:0 ~f:(fun acc line ->
    let first_num = Advent.Day1_utils.find_first_num 0 1 line in
    let second_num =
      Advent.Day1_utils.find_first_num (String.length line - 1) (-1) line
    in
    match first_num, second_num with
    | Some f, Some s ->
      let number = Int.of_string (Char.to_string f ^ Char.to_string s) in
      acc + number
    | _, _ -> 0)
  |> Fmt.pr "Result: %d@."
;;
