open Core
module StringSet = Set.Make (String)

exception Invalid_input

let add_up_row row =
  match String.split_on_chars row ~on:[ ':'; '|' ] with
  | _first_piece :: winning_numbers :: my_numbers :: _rest ->
    let winning_set =
      StringSet.of_list
        (List.filter (String.split winning_numbers ~on:' ') ~f:(fun s ->
           not (String.is_empty s)))
    in
    List.length
      (List.filter (String.split my_numbers ~on:' ') ~f:(fun num ->
         StringSet.mem winning_set num))
  | _ -> raise Invalid_input
;;

let rec process_data_part_1 lines =
  match lines with
  | [] -> 0
  | row :: body ->
    let result = add_up_row row in
    let result = if result = 0 then 0 else Int.pow 2 (result - 1) in
    result + process_data_part_1 body
;;

let increment_array array increment_by = Array.map array ~f:(fun x -> x + increment_by)

let rec process_data_part_2 lines results i =
  match lines with
  | [] -> 0
  | row :: body ->
    let result = add_up_row row in
    let num_current_card = Array.get results i in
    let () =
      if result <> 0
      then
        Array.blit
          ~src:
            (increment_array
               (Array.sub results ~pos:(i + 1) ~len:result)
               num_current_card)
          ~src_pos:0
          ~dst:results
          ~dst_pos:(i + 1)
          ~len:result
    in
    num_current_card + process_data_part_2 body results (i + 1)
;;

let lines = Advent.Advent_tools.read_lines "./input/puzzle_4.txt"
let () = process_data_part_1 lines |> Fmt.pr "Result: %d@."
let results = Array.create ~len:(List.length lines) 1
let () = process_data_part_2 lines results 0 |> Fmt.pr "Result: %d@."
