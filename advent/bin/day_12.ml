open Core

type spring_record =
  { puzzle : char list
  ; spring_num : int list
  }

let rec fill_base_case puzzle dp valid =
  match puzzle with
  | [] -> dp
  | s :: puzzle ->
    let valid = valid && (Char.equal s '.' || Char.equal s '?') in
    let dp = if not valid then dp @ [ 0 ] else dp @ [ 1 ] in
    fill_base_case puzzle dp valid
;;

let nth list pos =
  match List.nth list pos with
  | None -> 0
  | Some x -> x
;;

let try_match full_puzzle dp_previous pos spring current_group =
  let prev_c =
    match List.nth full_puzzle (pos - spring) with
    | None -> ' '
    | Some s -> s
  in
  let additional_length = if pos - spring >= 0 && Char.equal prev_c '?' then 1 else 0 in
  let can_match =
    (not (Char.equal prev_c '#')) && current_group + 1 >= spring + additional_length
  in
  if can_match then nth dp_previous (pos - spring - additional_length) else 0
;;

let rec solve_puzzle full_puzzle spring puzzle dp_previous dp_current pos current_group =
  match puzzle with
  | [] -> dp_current
  | c :: puzzle ->
    let next_value =
      match c with
      | '.' -> nth dp_current (pos - 1)
      | '#' -> try_match full_puzzle dp_previous pos spring current_group
      | '?' ->
        try_match full_puzzle dp_previous pos spring current_group
        + nth dp_current (pos - 1)
      | _ -> raise (Invalid_argument "not possible")
    in
    let dp_current = dp_current @ [ next_value ] in
    (solve_puzzle [@tailcall])
      full_puzzle
      spring
      puzzle
      dp_previous
      dp_current
      (pos + 1)
      (if Char.equal c '?' || Char.equal c '#' then current_group + 1 else 0)
;;

let rec solve_springs puzzle springs dp =
  match springs with
  | [] -> dp
  | spring :: springs ->
    let dp = solve_puzzle puzzle spring puzzle dp [] 0 0 in
    solve_springs puzzle springs dp
;;

let solve record =
  let record = { puzzle = record.puzzle @ [ '.' ]; spring_num = record.spring_num } in
  let dp = fill_base_case (List.rev record.puzzle) [] true in
  let dp = solve_springs (List.rev record.puzzle) (List.rev record.spring_num) dp in
  List.last_exn dp
;;

let solve_five record =
  let record =
    { puzzle =
        record.puzzle
        @ [ '?' ]
        @ record.puzzle
        @ [ '?' ]
        @ record.puzzle
        @ [ '?' ]
        @ record.puzzle
        @ [ '?' ]
        @ record.puzzle
        @ [ '.' ]
    ; spring_num =
        record.spring_num
        @ record.spring_num
        @ record.spring_num
        @ record.spring_num
        @ record.spring_num
    }
  in
  let dp = fill_base_case (List.rev record.puzzle) [] true in
  let dp = solve_springs (List.rev record.puzzle) (List.rev record.spring_num) dp in
  List.last_exn dp
;;

let parse_line line result =
  let first_half, second_half =
    match String.split line ~on:' ' with
    | first_half :: second_half :: _ -> first_half, second_half
    | _ -> raise (Invalid_argument "failed to split line in half")
  in
  let record =
    { puzzle = String.to_list first_half
    ; spring_num =
        List.map (String.split second_half ~on:',') ~f:(fun c -> Int.of_string c)
    }
  in
  result @ [ record ]
;;

let rec parse_grid lines result =
  match lines with
  | [] -> result
  | line :: body ->
    let result = parse_line line result in
    parse_grid body result
;;

let lines = Advent.Advent_tools.read_lines "./input/puzzle12.txt"
let grid = parse_grid lines []
let t = Time_ns.to_int_ns_since_epoch (Time_ns.now ())
let result = List.fold grid ~init:0 ~f:(fun acc record -> acc + solve record)
let t_one = Time_ns.to_int_ns_since_epoch (Time_ns.now ())
let () = Fmt.pr "Result: %d in %d \n" result (t_one - t)
let t = Time_ns.to_int_ns_since_epoch (Time_ns.now ())
let result = List.fold grid ~init:0 ~f:(fun acc record -> acc + solve_five record)
let t_two = Time_ns.to_int_ns_since_epoch (Time_ns.now ())
let () = Fmt.pr "Result: %d in %d " result (t_two - t)
