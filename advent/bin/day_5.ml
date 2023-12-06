open Core

exception Invalid_input

let rec build_map ~lines ~current_map =
  match lines with
  | "" :: body -> body, current_map
  | [] -> lines, current_map
  | string :: body ->
    let v =
      match String.split string ~on:' ' with
      | first :: second :: third :: _ ->
        let first_num = Int.of_string first in
        let second_num = Int.of_string second in
        let third_num = Int.of_string third in
        (second_num, second_num + third_num), first_num - second_num
      | _ -> raise Invalid_input
    in
    build_map ~lines:body ~current_map:(current_map @ [ v ])
;;

let rec build_maps ~lines ~maps =
  match lines with
  | [] -> maps
  | _ :: body ->
    let l, c_map = build_map ~lines:body ~current_map:[] in
    build_maps ~lines:l ~maps:(maps @ [ c_map ])
;;

let transform_seed_and_get_bound seed map next_bound =
  let op, temp_bound =
    match
      List.find map ~f:(fun ((x, y), _) -> if seed >= x && seed < y then true else false)
    with
    | Some ((_, bound), op) -> op, bound - seed
    | _ ->
      ( 0
      , List.fold map ~init:Int.max_value ~f:(fun bound ((x, _), _) ->
          if x - seed < bound then x - seed else bound) )
  in
  seed + op, if temp_bound < next_bound then temp_bound else next_bound
;;

let rec transform_seeds_with_maps seed maps next_bound =
  match maps with
  | [] -> seed, next_bound
  | map :: body ->
    let seed, next_bound = transform_seed_and_get_bound seed map next_bound in
    transform_seeds_with_maps seed body next_bound
;;

let rec transform_range current stop min maps =
  let seed, next_bound = transform_seeds_with_maps current maps stop in
  let min = if seed < min then seed else min in
  if stop = 0
  then min
  else if next_bound > stop || next_bound <= 0
  then transform_range (current + stop) 0 min maps
  else transform_range (current + next_bound) (stop - next_bound) min maps
;;

let rec transform seeds maps min =
  match seeds with
  | [] -> min
  | _ :: [] -> raise Invalid_input
  | one :: two :: body ->
    let min = transform_range one (two - 1) min maps in
    transform body maps min
;;

let seeds, maps =
  let seeds, body =
    match Advent.Advent_tools.read_lines "./input/puzzle5.txt" with
    | seeds :: _ :: body -> seeds, body
    | _ -> raise Invalid_input
  in
  let seeds =
    match String.split seeds ~on:' ' with
    | _ :: seeds -> seeds
    | _ -> raise Invalid_input
  in
  let raw_seeds = List.map seeds ~f:(fun x -> Int.of_string x) in
  let maps = build_maps ~lines:body ~maps:[] in
  raw_seeds, maps
;;

let () =
  let new_seeds =
    List.map seeds ~f:(fun seed ->
      let seed, _ = transform_seeds_with_maps seed maps Int.max_value in
      seed)
  in
  List.hd_exn (List.sort new_seeds ~compare:(fun a b -> a - b)) |> Fmt.pr "Result: %d@."
;;

let () =
  let result = transform seeds maps Int.max_value in
  result |> Fmt.pr "Result: %d@."
;;
