open Core

let () =
  let lines = Advent.read_lines "./input/puzzle_1.txt" in
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
