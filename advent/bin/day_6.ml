open Core

let calc s time = s * (time - s)

let rec check_edges s time record op =
  if calc s time <= record then s - op else check_edges (s + op) time record op
;;

let solve_quadratic a time c record =
  let a = Float.of_int a in
  let b = Float.of_int time in
  let c = Float.of_int c in
  let sqrt = Float.sqrt ((b *. b) -. (4. *. a *. c)) in
  let s1 = Int.of_float ((-.b +. sqrt) /. (2. *. a)) + 1 in
  let s2 = Int.of_float ((-.b -. sqrt) /. (2. *. a)) - 1 in
  let s1 = check_edges s1 time record (-1) in
  let s2 = check_edges s2 time record 1 in
  s1, s2
;;

let lines = Advent.Advent_tools.read_lines "./input/puzzle6.txt"

let races =
  match lines with
  | [ first; second ] ->
    let times =
      match
        List.filter (String.split first ~on:' ') ~f:(fun x -> not (String.is_empty x))
      with
      | _ :: times -> times
      | [] -> raise (Invalid_argument "Empty Times")
    in
    let distances =
      match
        List.filter (String.split second ~on:' ') ~f:(fun x -> not (String.is_empty x))
      with
      | _ :: distances -> distances
      | [] -> raise (Invalid_argument "Empty Distances")
    in
    (match List.zip times distances with
     | Ok a -> a
     | _ -> raise (Invalid_argument "ahh"))
  | _ -> raise (Invalid_argument "Unbalances times and distance lists ")
;;

let () =
  List.fold races ~init:1 ~f:(fun acc (time, record) ->
    let time = Int.of_string time in
    let record = Int.of_string record in
    let r1, r2 = solve_quadratic (-1) time (-record + 1) record in
    (r2 - r1 + 1) * acc)
  |> Fmt.pr "Result: %d@."
;;

let () =
  let time, record =
    List.fold races ~init:("", "") ~f:(fun (t, d) (tt, dd) -> t ^ tt, d ^ dd)
  in
  let time = Int.of_string time in
  let record = Int.of_string record in
  let r1, r2 = solve_quadratic (-1) time (-record + 1) record in
  r2 - r1 + 1 |> Fmt.pr "Result: %d@."
;;
