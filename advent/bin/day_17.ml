open Core

module Cord = struct
  module T = struct
    type t = int * int * int * int * int [@@deriving sexp_of, compare, sexp]
  end

  include T
  include Comparable.Make (T)
end

module CordMap = Map.Make (Cord)

let is_valid_cord grid y x z =
  if y < 0 || x < 0 || y >= Array.length grid || x >= Array.length grid.(0) || z > 2
  then None
  else Some grid.(y).(x)
;;

let update_adj heap full_map grid (y, x, z) (dy, dx) v =
  match is_valid_cord grid y x z with
  | None -> heap, full_map
  | Some dv ->
    let path_length = dv + v in
    (match CordMap.mem full_map (y, x, z, dy, dx) with
     | true -> heap, full_map
     | false ->
       let () = Pairing_heap.add heap ((y, x, z, dy, dx), path_length) in
       heap, full_map)
;;

let rec dijkstra grid heap full_map =
  match Pairing_heap.pop heap with
  | None -> raise (Invalid_argument "couldn't find finish")
  | Some ((y, x, z, dy, dx), v) ->
    if CordMap.mem full_map (y, x, z, dy, dx)
    then dijkstra grid heap full_map
    else (
      let full_map = CordMap.add_exn full_map ~key:(y, x, z, dy, dx) ~data:v in
      if y = Array.length grid - 1 && x = Array.length grid.(0) - 1
      then v
      else (
        let heap, full_map =
          (*start of process*)
          if dx = 0 && dy = 0
          then (
            let heap, full_map = update_adj heap full_map grid (y + 1, x, 0) (1, 0) v in
            update_adj heap full_map grid (y, x + 1, 0) (0, 1) v)
          else (
            let heap, full_map =
              update_adj heap full_map grid (y + dx, x + dy, 0) (dx, dy) v
            in
            let heap, full_map =
              update_adj heap full_map grid (y - dx, x - dy, 0) (-dx, -dy) v
            in
            update_adj heap full_map grid (y + dy, x + dx, z + 1) (dy, dx) v)
        in
        dijkstra grid heap full_map))
;;

let grid = Array.of_list (Advent.Advent_tools.read_lines "./input/puzzle17.txt")
let grid = Array.map grid ~f:(fun l -> String.to_array l)
let grid = Array.map grid ~f:(fun row -> Array.map row ~f:(fun c -> Char.get_digit_exn c))

let heap =
  Pairing_heap.create
    ?min_size:(Some (Array.length grid * 3))
    ~cmp:(fun ((_, _, _, _, _), v1) ((_, _, _, _, _), v2) -> v1 - v2)
    ()
;;

let () = Pairing_heap.add heap ((0, 0, 0, 0, 0), 0)
let result = dijkstra grid heap CordMap.empty
let () = Fmt.pr "result: %d" result
