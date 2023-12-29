open Core

module Cord = struct
  module T = struct
    type t = int * int * int * int * int [@@deriving sexp_of, compare, sexp]
  end

  include T
  include Comparable.Make (T)
end

module CordMap = Map.Make (Cord)

let is_valid_cord grid y x z limit =
  if y < 0 || x < 0 || y >= Array.length grid || x >= Array.length grid.(0) || z > limit
  then None
  else Some grid.(y).(x)
;;

let update_adj heap full_map grid (y, x, z) (dy, dx) v limit =
  match is_valid_cord grid y x z limit with
  | None -> ()
  | Some dv ->
    let path_length = dv + v in
    (match CordMap.mem full_map (y, x, z, dy, dx) with
     | true -> ()
     | false -> Pairing_heap.add heap ((y, x, z, dy, dx), path_length))
;;

let visit_adjacnet grid heap full_map y x z dx dy v z_min z_limit =
  (*start of process*)
  if dx = 0 && dy = 0
  then (
    let () = update_adj heap full_map grid (y + 1, x, 0) (1, 0) v z_limit in
    update_adj heap full_map grid (y, x + 1, 0) (0, 1) v 9)
  else (
    let () =
      if z < z_min
      then ()
      else (
        let () = update_adj heap full_map grid (y + dx, x + dy, 0) (dx, dy) v z_limit in
        update_adj heap full_map grid (y - dx, x - dy, 0) (-dx, -dy) v z_limit)
    in
    update_adj heap full_map grid (y + dy, x + dx, z + 1) (dy, dx) v z_limit)
;;

let rec dijkstra grid heap full_map z_min z_limit =
  match Pairing_heap.pop heap with
  | None -> raise (Invalid_argument "couldn't find finish")
  | Some ((y, x, z, dy, dx), v) ->
    if CordMap.mem full_map (y, x, z, dy, dx)
    then dijkstra grid heap full_map z_min z_limit
    else (
      let full_map = CordMap.add_exn full_map ~key:(y, x, z, dy, dx) ~data:v in
      if y = Array.length grid - 1 && x = Array.length grid.(0) - 1 && z >= z_min
      then v
      else (
        let () = visit_adjacnet grid heap full_map y x z dx dy v z_min z_limit in
        dijkstra grid heap full_map z_min z_limit))
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
let result = dijkstra grid heap CordMap.empty 0 2
let () = Fmt.pr "result: %d\n" result

let heap =
  Pairing_heap.create
    ?min_size:(Some (Array.length grid * 3))
    ~cmp:(fun ((_, _, _, _, _), v1) ((_, _, _, _, _), v2) -> v1 - v2)
    ()
;;

let () = Pairing_heap.add heap ((0, 0, 0, 0, 0), 0)
let result = dijkstra grid heap CordMap.empty 3 9
let () = Fmt.pr "result: %d\n" result
