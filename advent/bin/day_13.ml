open Core

type puzzle =
  { rows : int list
  ; cols : int array
  }

let rec next_puzzle_input (lines : string list) acc =
  match lines with
  | [] -> acc, []
  | "" :: lines -> acc, lines
  | line :: lines ->
    let acc = acc @ [ String.to_list line ] in
    next_puzzle_input lines acc
;;

let rec parse_puzzle lines output =
  match lines with
  | [] -> output
  | line :: lines ->
    let row_nums = output.rows in
    let col_nums = output.cols in
    let row_num =
      List.foldi line ~init:0 ~f:(fun i row_num c ->
        match c with
        | '#' ->
          let row_num = Int.( lsl ) row_num 1 + 1 in
          let () = col_nums.(i) <- Int.( lsl ) col_nums.(i) 1 + 1 in
          row_num
        | '.' ->
          let row_num = Int.( lsl ) row_num 1 in
          let () = col_nums.(i) <- Int.( lsl ) col_nums.(i) 1 in
          row_num
        | _ -> raise (Invalid_argument "invalich char"))
    in
    let output = { rows = row_nums @ [ row_num ]; cols = col_nums } in
    parse_puzzle lines output
;;

let rec parse_puzzles lines (acc : puzzle list) =
  match lines with
  | [] -> acc
  | lines ->
    let puzzle_list, lines = next_puzzle_input lines [] in
    let puzzle =
      parse_puzzle
        puzzle_list
        { rows = []
        ; cols = Array.init (List.length (List.hd_exn puzzle_list)) ~f:(fun _ -> 0)
        }
    in
    let acc = acc @ [ puzzle ] in
    parse_puzzles lines acc
;;

let rec lists_match a b allow_smudge =
  match a with
  | [] -> true && not allow_smudge
  | a1 :: a ->
    (match b with
     | [] -> true && not allow_smudge
     | b1 :: b ->
       if a1 <> b1
       then (
         let diff = Int.bit_xor a1 b1 in
         if allow_smudge && Int.ceil_log2 diff = Int.floor_log2 diff
         then (lists_match [@tailcall]) a b false
         else false)
       else (lists_match [@tailcall]) a b allow_smudge)
;;

let rec find_mirror list previous allow_smudge =
  match list with
  | [] -> 0
  | _ :: [] -> 0
  | a :: list ->
    let previous = a :: previous in
    (match lists_match previous list allow_smudge with
     | true -> List.length previous
     | false -> (find_mirror [@tailcall]) list previous allow_smudge)
;;

let solve next_puzzle allow_smudge =
  let row_solution = find_mirror next_puzzle.rows [] allow_smudge in
  let col_solution =
    if row_solution = 0
    then find_mirror (Array.to_list next_puzzle.cols) [] allow_smudge
    else 0
  in
  (100 * row_solution) + col_solution
;;

let lines = Advent.Advent_tools.read_lines "./input/puzzle13.txt"
let puzzles = parse_puzzles lines []
let t = Time_ns.to_int_ns_since_epoch (Time_ns.now ())
let result = List.fold puzzles ~init:0 ~f:(fun acc puzzle -> acc + solve puzzle false)
let t_d = Time_ns.to_int_ns_since_epoch (Time_ns.now ())
let () = Fmt.pr "Result %d in %d \n" result (t_d - t)
let t = Time_ns.to_int_ns_since_epoch (Time_ns.now ())
let result = List.fold puzzles ~init:0 ~f:(fun acc puzzle -> acc + solve puzzle true)
let t_d = Time_ns.to_int_ns_since_epoch (Time_ns.now ())
let () = Fmt.pr "Result %d in %d \n" result (t_d - t)