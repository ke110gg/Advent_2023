open Core

let rec calc_differences nums acc =
  match nums with
  | [] -> acc
  | _ :: [] -> acc
  | a :: b :: body -> calc_differences ([ b ] @ body) (acc @ [ b - a ])
;;

let rec end_state list =
  match list with
  | [] -> true
  | head :: body -> if head <> 0 then false else end_state body
;;

let rec predict_next list acc =
  let differences = calc_differences list [] in
  (**let () = Fmt.pr "differences \n" in
  let () = List.iter differences ~f:(fun x -> Fmt.pr "%d," x) in
  let () = Fmt.pr "\n" in**)
  if end_state differences
  then (
    (**let () = Fmt.pr "aggregate \n" in**)
    List.last_exn list
    + List.fold acc ~init:0 ~f:(fun x y ->
      (**let () = Fmt.pr "%d\n" y in**)
      x + y))
  else predict_next differences (acc @ [ List.last_exn list ])
;;

let lines = Advent.Advent_tools.read_lines "./input/puzzle9.txt"

let lines =
  List.map lines ~f:(fun l ->
    List.map (String.split l ~on:' ') ~f:(fun s -> Int.of_string s))
;;

let results = List.map lines ~f:(fun l -> predict_next l []);;

List.fold results ~init:0 ~f:(fun x acc ->
  (**let () = Fmt.pr "%d\n" x in**)
  x + acc)
|> Fmt.pr "Result: %d@"
