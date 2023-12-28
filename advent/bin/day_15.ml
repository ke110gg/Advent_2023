open Core

let rec hash string_list acc =
  match string_list with
  | [] -> acc
  | c :: list ->
    let acc = acc + Char.to_int c in
    let acc = acc * 17 % 256 in
    hash list acc
;;

let process_step boxes step =
  let remove = String.mem step '-' in
  let split = String.split_on_chars step ~on:[ '-'; '=' ] in
  let hash_string, focal =
    match split with
    | [ hash_string; focal ] -> hash_string, if remove then 0 else Int.of_string focal
    | _ -> raise (Invalid_argument "not possible")
  in
  let hash_value = hash (String.to_list hash_string) 0 in
  let () =
    match remove with
    | false ->
      boxes.(hash_value)
      <- (match boxes.(hash_value) with
          | [] -> [ hash_string, focal ]
          | list ->
            if List.Assoc.mem list ~equal:(fun a b -> String.equal a b) hash_string
            then
              List.map list ~f:(fun (key, value) ->
                if String.equal key hash_string then key, focal else key, value)
            else list @ [ hash_string, focal ])
    | true ->
      boxes.(hash_value)
      <- (match boxes.(hash_value) with
          | [] -> []
          | list ->
            List.Assoc.remove list ~equal:(fun a b -> String.equal a b) hash_string)
  in
  boxes
;;

let line =
  String.filter (Advent.Advent_tools.read_line "./input/puzzle15.txt") ~f:(fun c ->
    not (Char.equal c '\n'))
;;

let steps = String.split ~on:',' line

let result =
  List.fold steps ~init:0 ~f:(fun acc x ->
    let r = hash (String.to_list x) 0 in
    r + acc)
;;

let () = Fmt.pr "result: %d\n" result
let boxes = Array.init 256 ~f:(fun _ -> [])
let boxes = List.fold steps ~init:boxes ~f:(fun boxes step -> process_step boxes step)

let result =
  Array.foldi boxes ~init:0 ~f:(fun j acc list ->
    List.foldi list ~init:acc ~f:(fun i acc (_, value) ->
      let temp = (j + 1) * (i + 1) * value in
      acc + temp))
;;

let () = Fmt.pr "result: %d\n" result
