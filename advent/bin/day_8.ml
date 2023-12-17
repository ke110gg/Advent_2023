open Core
module StringMap = Map.Make (String)

type direction_type =
  | RIGHT
  | LEFT

let parse_directions line =
  let chars = String.to_list line in
  List.map chars ~f:(fun x ->
    match x with
    | 'R' -> RIGHT
    | 'L' -> LEFT
    | _ -> raise (Invalid_argument "Invalid direction"))
;;

let rec parse_map lines map =
  match lines with
  | [] -> map
  | "" :: body -> parse_map body map
  | line :: body ->
    let node, left_node, right_node =
      Scanf.sscanf line "%s@ = (%s@, %s@)" (fun a b c -> a, b, c)
    in
    parse_map body (StringMap.set map ~key:node ~data:(left_node, right_node))
;;

let part_2_comparator string =
  let pos = String.get string 2 in
  Char.equal pos 'Z'
;;

let part_1_comparator string = String.equal string "ZZZ"

let rec run_iteration directions map acc current_point comparator =
  match directions with
  | [] -> current_point, acc
  | d :: directions ->
    let left_node, right_node =
      match StringMap.find map current_point with
      | None -> raise (Invalid_argument "missing node")
      | Some a -> a
    in
    let current_point =
      match d with
      | RIGHT -> right_node
      | LEFT -> left_node
    in
    if comparator current_point
    then current_point, acc + 1
    else run_iteration directions map (acc + 1) current_point comparator
;;

let rec run_route directions map acc current_point comparator =
  let current_point, acc = run_iteration directions map acc current_point comparator in
  if comparator current_point
  then acc
  else run_route directions map acc current_point comparator
;;

let rec gcd a b = if b = 0 then a else gcd b (a % b)

let rec lcm nums =
  match nums with
  | [] -> raise (Invalid_argument "ahh")
  | a :: [] -> a
  | a :: b :: body ->
    let new_list = [ a / gcd a b * b ] @ body in
    lcm new_list
;;

let lines = Advent.Advent_tools.read_lines "./input/puzzle8.txt"

let directions, map =
  match lines with
  | first :: body -> parse_directions first, parse_map body StringMap.empty
  | [] -> [], StringMap.empty
;;

let starting_points =
  StringMap.fold map ~init:[] ~f:(fun ~key ~data:_ acc ->
    if Char.( <> ) (String.get key 2) 'A' then acc else acc @ [ key ])
;;

let result = run_route directions map 0 "AAA" part_1_comparator
let () = Fmt.pr "result: %d\n" result

let result =
  List.map starting_points ~f:(fun p -> run_route directions map 0 p part_2_comparator)
;;

let () = lcm result |> Fmt.pr "result: %d\n"
