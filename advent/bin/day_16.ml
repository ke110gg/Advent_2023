open Core

module Cord = struct
  module T = struct
    type t = (int * int) * (int * int) [@@deriving sexp_of, compare, sexp]
  end

  include T
  include Comparable.Make (T)
end

module CordSet = Set.Make (Cord)

module IntPair = struct
  module T = struct
    type t = int * int [@@deriving sexp_of, compare, sexp]
  end

  include T
  include Comparable.Make (T)
end

module IntPairSet = Set.Make (IntPair)

let read_grid grid y x =
  if y < 0 || x < 0 || y >= Array.length grid || x >= Array.length grid.(0)
  then None
  else Some grid.(y).(x)
;;

let drop_delta set =
  CordSet.fold set ~init:IntPairSet.empty ~f:(fun acc ((x, y), (_, _)) ->
    IntPairSet.add acc (x, y))
;;

let get_starting_points grid =
  let y_length = Array.length grid in
  let x_length = Array.length grid.(0) in
  let starting_points =
    Array.foldi grid ~init:[] ~f:(fun i acc _ ->
      acc @ [ (i, 0), (0, 1); (i, x_length - 1), (0, -1) ])
  in
  Array.foldi grid.(0) ~init:starting_points ~f:(fun i acc _ ->
    acc @ [ (0, i), (1, 0); (y_length - 1, i), (-1, 0) ])
;;

let rec follow_path grid queue cords =
  match queue with
  | [] -> cords
  | current :: queue ->
    let (y, x), (dy, dx) = current in
    if CordSet.mem cords current
    then follow_path grid queue cords
    else (
      match read_grid grid y x with
      | None -> follow_path grid queue cords
      | Some c ->
        let cords = CordSet.add cords current in
        (match c with
         | '.' -> follow_path grid (queue @ [ (y + dy, x + dx), (dy, dx) ]) cords
         | '\\' -> follow_path grid (queue @ [ (y + dx, x + dy), (dx, dy) ]) cords
         | '/' -> follow_path grid (queue @ [ (y - dx, x - dy), (-dx, -dy) ]) cords
         | '-' ->
           if dy = 0
           then follow_path grid (queue @ [ (y + dy, x + dx), (dy, dx) ]) cords
           else
             follow_path grid (queue @ [ (y, x + 1), (0, 1); (y, x - 1), (0, -1) ]) cords
         | '|' ->
           if dx = 0
           then follow_path grid (queue @ [ (y + dy, x + dx), (dy, dx) ]) cords
           else
             follow_path
               grid
               (queue @ [ (y + 1, x + 0), (1, 0); (y - 1, x + 0), (-1, 0) ])
               cords
         | _ -> raise (Invalid_argument "")))
;;

let get_energized grid start =
  let set = follow_path grid start CordSet.empty in
  let set = drop_delta set in
  IntPairSet.length set
;;

let grid = Array.of_list (Advent.Advent_tools.read_lines "./input/puzzle16.txt")
let grid = Array.map grid ~f:(fun l -> String.to_array l)
let result = get_energized grid [ (0, 0), (0, 1) ]
let () = Fmt.pr "result: %d\n" result
let starting_points = get_starting_points grid

let result =
  List.fold starting_points ~init:0 ~f:(fun acc point ->
    let result = get_energized grid [ point ] in
    if result > acc then result else acc)
;;

let () = Fmt.pr "result: %d\n" result
