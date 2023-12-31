open Core

module Symbol = struct
  type t = int * int [@@deriving sexp]

  let compare (x0, y0) (x1, y1) =
    match Stdlib.compare x0 x1 with
    | 0 -> Stdlib.compare y0 y1
    | c -> c
  ;;
end

module PairsMap = Map.Make (Symbol)

let parse_line line line_num ~numbers ~symbols =
  let line = String.to_list line in
  let numbers, symbols, i, current_num =
    List.fold
      line
      ~init:(numbers, symbols, 0, "")
      ~f:(fun (numbers, symbols, i, current_num) col ->
        let numbers, symbols, current_num =
          match col with
          | '.' ->
            let numbers =
              if String.length current_num > 0
              then numbers @ [ line_num, i - String.length current_num, current_num ]
              else numbers
            in
            numbers, symbols, ""
          | _ ->
            if Char.is_digit col
            then numbers, symbols, current_num ^ String.of_char col
            else if String.length current_num > 0
            then
              ( numbers @ [ line_num, i - String.length current_num, current_num ]
              , PairsMap.add_exn symbols ~key:(line_num, i) ~data:(0, col)
              , "" )
            else numbers, PairsMap.add_exn symbols ~key:(line_num, i) ~data:(0, col), ""
        in
        numbers, symbols, i + 1, current_num)
  in
  if String.length current_num > 0
  then numbers @ [ line_num, i - String.length current_num, current_num ], symbols
  else numbers, symbols
;;

let maybe_add_number (r, c, value) symbols =
  let left_bound = c - 1 in
  let right_bound = 1 + c + String.length value in
  let acc =
    List.fold
      [ -1 + r; r; 1 + r ]
      ~init:0
      ~f:(fun acc r ->
        List.fold (List.range left_bound right_bound) ~init:acc ~f:(fun acc c ->
          if acc > 0
          then acc
          else if PairsMap.mem symbols (r, c)
          then Int.of_string value
          else acc))
  in
  acc
;;

let maybe_add_number_part_2 (r, c, value) symbols =
  let left_bound = c - 1 in
  let right_bound = 1 + c + String.length value in
  let acc =
    List.fold
      [ -1 + r; r; 1 + r ]
      ~init:(0, symbols)
      ~f:(fun (acc, symbols) r ->
        List.fold
          (List.range left_bound right_bound)
          ~init:(acc, symbols)
          ~f:(fun (acc, symbols) c ->
            match PairsMap.find symbols (r, c) with
            | None -> acc, symbols
            | Some (v, ch) ->
              (match ch with
               | '*' ->
                 if v = 0
                 then acc, PairsMap.set symbols ~key:(r, c) ~data:(Int.of_string value, ch)
                 else if v < 0
                 then acc - v, PairsMap.remove symbols (r, c)
                 else
                   ( acc + (v * Int.of_string value)
                   , PairsMap.set
                       symbols
                       ~key:(r, c)
                       ~data:(-1 * v * Int.of_string value, ch) )
               | _ -> acc, symbols)))
  in
  acc
;;

let rec parse_data lines line_num ~numbers ~symbols =
  match lines with
  | [] -> numbers, symbols
  | head :: body ->
    let numbers, symbols = parse_line head line_num ~numbers ~symbols in
    parse_data body (line_num + 1) ~numbers ~symbols
;;

let rec process_data ~acc ~numbers ~symbols =
  match numbers with
  | [] -> acc
  | number :: body ->
    let acc = acc + maybe_add_number number symbols in
    process_data ~acc ~numbers:body ~symbols
;;

let rec process_data_part2 ~acc ~numbers ~symbols =
  match numbers with
  | [] -> acc
  | number :: body ->
    let temp_acc, symbols = maybe_add_number_part_2 number symbols in
    let acc = acc + temp_acc in
    process_data_part2 ~acc ~numbers:body ~symbols
;;

let lines = Advent.Advent_tools.read_lines "./input/puzzle3.txt"

let () =
  let numbers, symbols = parse_data lines 0 ~numbers:[] ~symbols:PairsMap.empty in
  process_data ~acc:0 ~numbers ~symbols |> Fmt.pr "Result: %d@."
;;

let () =
  let numbers, symbols = parse_data lines 0 ~numbers:[] ~symbols:PairsMap.empty in
  process_data_part2 ~acc:0 ~numbers ~symbols |> Fmt.pr "Result: %d@."
;;