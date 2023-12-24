open Core

let rec combine list acc =
  match list with
  | [] -> acc
  | a :: list ->
    let acc = acc @ List.fold list ~init:[] ~f:(fun acc l -> acc @ [ a, l ]) in
    combine list acc
;;

let lines = Advent.Advent_tools.read_lines "./input/puzzle11.txt"
let grid = List.map lines ~f:(fun l -> String.to_list l)
let x_galaxies = Array.create ~len:(List.length (List.hd_exn grid)) 0
let y_galaxies = Array.create ~len:(List.length grid) 0

let y_galaxies, x_galaxies =
  List.foldi grid ~init:(y_galaxies, x_galaxies) ~f:(fun y acc row ->
    List.foldi row ~init:acc ~f:(fun x (y_galaxies, x_galaxies) item ->
      let () =
        if Char.equal item '#'
        then (
          let () = y_galaxies.(y) <- y_galaxies.(y) + 1 in
          x_galaxies.(x) <- x_galaxies.(x) + 1)
      in
      y_galaxies, x_galaxies))
;;

let x_galaxies =
  Array.foldi x_galaxies ~init:[] ~f:(fun i acc count ->
    if count = 0 then acc @ [ i ] else acc)
;;

let y_galaxies =
  Array.foldi y_galaxies ~init:[] ~f:(fun i acc count ->
    if count = 0 then acc @ [ i ] else acc)
;;

let points =
  List.foldi grid ~init:[] ~f:(fun y acc row ->
    List.foldi row ~init:acc ~f:(fun x acc c ->
      if not (Char.equal c '#') then acc else acc @ [ y, x ]))
;;

let combinations = combine points []

let total =
  List.fold combinations ~init:0 ~f:(fun acc ((y1, x1), (y2, x2)) ->
    let x1, x2 = if x1 < x2 then x1, x2 else x2, x1 in
    let y1, y2 = if y1 < y2 then y1, y2 else y2, y1 in
    let acc = acc + y2 - y1 + x2 - x1 in
    let acc =
      List.fold x_galaxies ~init:acc ~f:(fun acc x ->
        if x1 < x && x < x2 then acc + 1 else acc)
    in
    List.fold y_galaxies ~init:acc ~f:(fun acc y ->
      if y1 < y && y < y2 then acc + 1 else acc))
;;

let () = Fmt.pr "Result %d\n" total

let total =
  List.fold combinations ~init:0 ~f:(fun acc ((y1, x1), (y2, x2)) ->
    let x1, x2 = if x1 < x2 then x1, x2 else x2, x1 in
    let y1, y2 = if y1 < y2 then y1, y2 else y2, y1 in
    let acc = acc + y2 - y1 + x2 - x1 in
    let acc =
      List.fold x_galaxies ~init:acc ~f:(fun acc x ->
        if x1 < x && x < x2 then acc + (1000000 - 1) else acc)
    in
    List.fold y_galaxies ~init:acc ~f:(fun acc y ->
      if y1 < y && y < y2 then acc + (1000000 - 1) else acc))
;;

let () = Fmt.pr "Result %d " total