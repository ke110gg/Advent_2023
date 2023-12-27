open Core

let matrix_to_string matrix =
  Array.fold matrix ~init:"" ~f:(fun acc row ->
    Array.fold row ~init:acc ~f:(fun acc c -> acc ^ String.of_char c) ^ "\n")
;;

let rec shift_item grid y x dy dx =
  if y < 0 || x < 0 || y >= Array.length grid || x >= Array.length grid.(0)
  then 'O'
  else (
    match grid.(y).(x) with
    | '#' -> 'O'
    | '.' ->
      let () = grid.(y).(x) <- shift_item grid (y + dy) (x + dx) dy dx in
      '.'
    | 'O' ->
      (match shift_item grid (y + dy) (x + dx) dy dx with
       | 'O' -> 'O'
       | '.' ->
         let () = grid.(y).(x) <- shift_item grid (y + dy) (x + dx) dy dx in
         '.'
       | _ -> raise (Invalid_argument "not possible"))
    | _ -> raise (invalid_arg "can't shift"))
;;

let shift grid dy dx =
  let () =
    Array.iteri grid ~f:(fun y row ->
      Array.iteri row ~f:(fun x c ->
        match c with
        | '#' -> ()
        | '.' -> ()
        | 'O' -> grid.(y).(x) <- shift_item grid (y + dy) (x + dx) dy dx
        | _ -> raise (Invalid_argument "invalid char")))
  in
  grid
;;

let score grid =
  let length = Array.length grid in
  let acc =
    Array.foldi grid ~init:0 ~f:(fun i acc row ->
      Array.fold row ~init:acc ~f:(fun acc c ->
        if Char.equal c 'O' then acc + (length - i) else acc))
  in
  acc
;;

let shift_north grid = shift grid (-1) 0
let shift_south grid = shift grid 1 0
let shift_east grid = shift grid 0 1
let shift_west grid = shift grid 0 (-1)

let peform_cycle grid =
  let grid = shift_north grid in
  let grid = shift_west grid in
  let grid = shift_south grid in
  shift_east grid
;;

let rec iterate_cycles grid i max table found_cycle =
  match i = max with
  | true -> grid
  | false ->
    let grid_string = matrix_to_string grid in
    (match Hashtbl.find table grid_string with
     | Some (new_grid, p) ->
       iterate_cycles
         new_grid
         (i + 1)
         (if not found_cycle then ((max - i) % (i - p)) + i else max)
         table
         true
     | None ->
       let new_grid = peform_cycle grid in
       let () =
         Hashtbl.add_exn table ~key:grid_string ~data:(Array.copy_matrix new_grid, i)
       in
       iterate_cycles new_grid (i + 1) max table false)
;;

let grid = Array.of_list (Advent.Advent_tools.read_lines "./input/puzzle14.txt")
let grid = Array.map grid ~f:(fun l -> String.to_array l)
let grid_part1 = shift_north grid
let result = score grid_part1
let () = Fmt.pr "Result %d" result
let table = Hashtbl.create ~size:10000 ~growth_allowed:true (module String)
let grid = iterate_cycles grid 0 1000000000 table false
let result = score grid
let () = Fmt.pr "Result %d" result
