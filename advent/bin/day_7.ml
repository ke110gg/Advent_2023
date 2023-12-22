open Core
module StringMap = Map.Make (String)

let value_map =
  [ 'A', "="
  ; 'K', "<"
  ; 'Q', ";"
  ; 'J', ":"
  ; 'T', "9"
  ; '9', "8"
  ; '8', "7"
  ; '7', "6"
  ; '6', "5"
  ; '5', "4"
  ; '4', "3"
  ; '3', "2"
  ; '2', "1"
  ; '1', "0"
  ]
;;

let value_map2 =
  [ 'A', "="
  ; 'K', "<"
  ; 'Q', ";"
  ; 'J', "/"
  ; 'T', "9"
  ; '9', "8"
  ; '8', "7"
  ; '7', "6"
  ; '6', "5"
  ; '5', "4"
  ; '4', "3"
  ; '3', "2"
  ; '2', "1"
  ; '1', "0"
  ]
;;

let rec frequency_count list map =
  match list with
  | [] -> map
  | l :: body ->
    let count =
      match StringMap.find map l with
      | None -> 1
      | Some x -> x + 1
    in
    let map = StringMap.set map ~key:l ~data:count in
    frequency_count body map
;;

let score_hand map =
  let higest_count =
    StringMap.fold map ~init:0 ~f:(fun ~key:_ ~data acc_count ->
      if data > acc_count then data else acc_count)
  in
  match StringMap.length map with
  | 1 -> 7
  | 2 ->
    (match higest_count with
     | 4 -> 6
     | 3 -> 5
     | _ -> raise (Invalid_argument "ahh"))
  | 3 ->
    (match higest_count with
     | 3 -> 4
     | 2 -> 3
     | _ -> raise (Invalid_argument "ahh"))
  | 4 -> 2
  | 5 -> 1
  | s -> raise (Invalid_argument (Fmt.str "ahh %d" s))
;;

let parse_hand line =
  let hand_string, bid =
    match String.split line ~on:' ' with
    | [ hand; bid ] -> hand, Int.of_string bid
    | _ -> raise (Invalid_argument "Parsing failed")
  in
  let mapped_list =
    List.map (String.to_list hand_string) ~f:(fun x ->
      let s = List.find value_map ~f:(fun (a, _) -> Char.equal a x) in
      match s with
      | Some (_, value) -> value
      | _ -> raise (Invalid_argument "ahh"))
  in
  let map = frequency_count mapped_list StringMap.empty in
  let hand_type = score_hand map in
  Int.to_string hand_type ^ List.fold ~init:"" mapped_list ~f:(fun acc x -> acc ^ x), bid
;;

let parse_hand_with_wild_card line =
  let hand_string, bid =
    match String.split line ~on:' ' with
    | [ hand; bid ] -> hand, Int.of_string bid
    | _ -> raise (Invalid_argument "Parsing failed")
  in
  let mapped_list =
    List.map (String.to_list hand_string) ~f:(fun x ->
      let s = List.find value_map2 ~f:(fun (a, _) -> Char.equal a x) in
      match s with
      | Some (_, value) -> value
      | _ -> raise (Invalid_argument "ahh"))
  in
  let map = frequency_count mapped_list StringMap.empty in
  let higest_count, highest_char =
    StringMap.fold map ~init:(0, "") ~f:(fun ~key ~data (acc_count, acc_char) ->
      if data > acc_count && String.( <> ) key "/" then data, key else acc_count, acc_char)
  in
  let map =
    StringMap.set
      map
      ~key:highest_char
      ~data:
        (higest_count
         +
         match StringMap.find map "/" with
         | None -> 0
         | Some a -> a)
  in
  let map = if String.( <> ) highest_char "/" then StringMap.remove map "/" else map in
  let hand_type = score_hand map in
  Int.to_string hand_type ^ List.fold ~init:"" mapped_list ~f:(fun acc x -> acc ^ x), bid
;;

let rec parse_hands lines hands parse_hands_fun =
  match lines with
  | [] -> hands
  | line :: body ->
    let hand = parse_hands_fun line in
    parse_hands body (List.append hands [ hand ]) parse_hands_fun
;;

let lines = Advent.Advent_tools.read_lines "./input/puzzle7.txt"
let hands = parse_hands lines [] parse_hand
let hands_with_wild_card = parse_hands lines [] parse_hand_with_wild_card
let sorted_hands = List.sort hands ~compare:(fun (a, _) (b, _) -> String.compare a b)

let sorted_hands_wildcard =
  List.sort hands_with_wild_card ~compare:(fun (a, _) (b, _) -> String.compare a b)
;;

let () =
  List.foldi sorted_hands ~init:0 ~f:(fun i acc (_, bid) ->
    (**let () = Fmt.pr "%s %d %d %d \n" a (i + 1) bid ((i + 1) * bid) in**)
    acc + ((i + 1) * bid))
  |> Fmt.pr "Result: %d@."
;;

let () =
  List.foldi sorted_hands_wildcard ~init:0 ~f:(fun i acc (_, bid) ->
    (**let () = Fmt.pr "%s %d %d %d \n" a (i + 1) bid ((i + 1) * bid) in**)
    acc + ((i + 1) * bid))
  |> Fmt.pr "Result: %d@."
;;
