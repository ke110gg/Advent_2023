open Core
module StringMap = Map.Make (String)

type direction =
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

let rec run_iteration directions map acc current_point =
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
    if String.equal current_point "ZZZ"
    then current_point, acc + 1
    else run_iteration directions map (acc + 1) current_point
;;

let rec run_route directions map acc current_point =
  let current_point, acc = run_iteration directions map acc current_point in
  match current_point with
  | "ZZZ" -> acc
  | _ -> run_route directions map acc current_point
;;

let lines = Advent.Advent_tools.read_lines "./input/puzzle8.txt"

let directions, map =
  match lines with
  | first :: body -> parse_directions first, parse_map body StringMap.empty
  | [] -> [], StringMap.empty
;;

let result = run_route directions map 0 "AAA"
let () = Fmt.pr "Result: %d@." result