open Core

module IntTrip = struct
  module T = struct
    type t = int * int * int [@@deriving sexp_of, compare, sexp]

    let hash = Hashtbl.hash
  end

  include T
  include Comparable.Make (T)
end

module IntTripSet = Set.Make (IntTrip)

type spot =
  | Empty
  | Stone
  | Start

let create_spot : char -> spot = function
  | '.' -> Empty
  | '#' -> Stone
  | 'S' -> Start
  | _ -> raise (Invalid_argument "inavlid input")
;;

let is_valid (x : int) (y : int) (grid : spot array array) (infinite_map : bool) =
  let y_length = Array.length grid in
  let x_length = Array.length grid.(0) in
  if not infinite_map
  then
    if x < 0 || y < 0 || x >= x_length || y >= y_length
    then false
    else (
      match grid.(y).(x) with
      | Stone -> false
      | _ -> true)
  else (
    let x, y = x % x_length, y % y_length in
    match grid.(y).(x) with
    | Stone -> false
    | _ -> true)
;;

let find_start (grid : spot array array) =
  Array.foldi grid ~init:(0, 0) ~f:(fun y acc row ->
    Array.foldi row ~init:acc ~f:(fun x acc a ->
      match a with
      | Start -> x, y
      | _ -> acc))
;;

let init_queue (queue : (int * int * int) list) (x : int) (y : int) =
  queue @ [ x - 1, y, 0; x + 1, y, 0; x, y - 1, 0; x, y + 1, 0 ]
;;

let append_to_queue
  (queue : (int * int * int) Base.Queue.t)
  (x : int)
  (y : int)
  (steps : int)
  =
  let steps = steps + 1 in
  Queue.enqueue_all
    queue
    [ x - 1, y, steps; x + 1, y, steps; x, y - 1, steps; x, y + 1, steps ]
;;

let rec walk
  (grid : spot array array)
  (queue : (int * int * int) Base.Queue.t)
  (visited : (int * int * int) Hash_set.t)
  (max_steps : int)
  (infinite_map : bool)
  (acc : int)
  =
  match Queue.dequeue queue with
  | None -> acc
  | Some (current_x, current_y, steps) ->
    if Hash_set.mem visited (current_x, current_y, steps)
    then walk grid queue visited max_steps infinite_map acc
    else (
      match is_valid current_x current_y grid infinite_map with
      | false -> walk grid queue visited max_steps infinite_map acc
      | true ->
        let acc = if steps + 1 = max_steps then acc + 1 else acc in
        let () =
          if steps + 1 < max_steps
          then append_to_queue queue current_x current_y steps
          else ()
        in
        let () = Hash_set.add visited (current_x, current_y, steps) in
        walk grid queue visited max_steps infinite_map acc)
;;

let grid =
  Array.of_list (Advent.Advent_tools.read_lines "./input/puzzle21.txt")
  |> Array.map ~f:(fun l -> String.to_array l)
  |> Array.map ~f:(fun row -> Array.map row ~f:(fun item -> create_spot item))
;;

let x, y = find_start grid
let (queue : (int * int * int) Base.Queue.t) = Queue.of_list (init_queue [] x y)

let () =
  walk
    grid
    (Queue.copy queue)
    (Hash_set.create ~growth_allowed:true ~size:10000 (module IntTrip))
    64
    false
    0
  |> Fmt.pr "part 1 result: %d\n"
;;

let () = Fmt.pr "Starting part 2"

let a0 =
  walk
    grid
    (Queue.copy queue)
    (Hash_set.create ~growth_allowed:true ~size:10000 (module IntTrip))
    65
    true
    0
;;

let () = Fmt.pr "a0: %d\n%!" a0

let a1 =
  walk
    grid
    (Queue.copy queue)
    (Hash_set.create ~growth_allowed:true ~size:10000 (module IntTrip))
    (65 + 131)
    true
    0
;;

let () = Fmt.pr "a1: %d\n%!" a1

let a2 =
  walk
    grid
    (Queue.copy queue)
    (Hash_set.create ~growth_allowed:true ~size:10000 (module IntTrip))
    (65 + (131 * 2))
    true
    0
;;

let () = Fmt.pr "a2: %d\n%!" a2
let a = (a0 - (2 * a1) + a2) / 2
let b = ((-3 * a0) + (4 * a1)) / 2
let c = a0
let x = 26501365 / 131
let () = Fmt.pr "when steps =26501365 plots=%d" ((a * x * x) + (b * x) + c)
