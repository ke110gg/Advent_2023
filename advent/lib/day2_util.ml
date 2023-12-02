open Core

type game =
  { red : int
  ; blue : int
  ; green : int
  }

let possible_hand = { red = 12; green = 13; blue = 14 }

let game_less game1 game2 =
  game1.red <= game2.red && game1.blue <= game2.blue && game1.green <= game2.green
;;

let add_game game1 game2 =
  { red = game1.red + game2.red
  ; blue = game1.blue + game2.blue
  ; green = game1.green + game2.green
  }
;;

let max_die game1 game2 =
  { red = (if game1.red > game2.red then game1.red else game2.red)
  ; blue = (if game1.blue > game2.blue then game1.blue else game2.blue)
  ; green = (if game1.green > game2.green then game1.green else game2.green)
  }
;;

exception Invalid_input

let rec valid_game list =
  match list with
  | [] -> true
  | string :: body ->
    let hand =
      List.fold
        (String.split string ~on:',')
        ~init:{ red = 0; blue = 0; green = 0 }
        ~f:(fun acc die ->
          let pieces = String.split die ~on:' ' in
          match pieces with
          | _ :: i :: s :: _ ->
            (match s with
             | "blue" -> add_game acc { red = 0; blue = Int.of_string i; green = 0 }
             | "red" -> add_game acc { red = Int.of_string i; blue = 0; green = 0 }
             | "green" -> add_game acc { green = Int.of_string i; blue = 0; red = 0 }
             | _ -> raise Invalid_input)
          | _ -> raise Invalid_input)
    in
    if not (game_less hand possible_hand) then false else valid_game body
;;

let rec calc_min_game list =
  match list with
  | [] -> { red = 0; blue = 0; green = 0 }
  | string :: body ->
    let hand =
      List.fold
        (String.split string ~on:',')
        ~init:{ red = 0; blue = 0; green = 0 }
        ~f:(fun acc die ->
          let pieces = String.split die ~on:' ' in
          match pieces with
          | _ :: i :: s :: _ ->
            (match s with
             | "blue" -> max_die acc { red = 0; blue = Int.of_string i; green = 0 }
             | "red" -> max_die acc { red = Int.of_string i; blue = 0; green = 0 }
             | "green" -> max_die acc { green = Int.of_string i; blue = 0; red = 0 }
             | _ -> raise Invalid_input)
          | _ -> raise Invalid_input)
    in
    max_die hand (calc_min_game body)
;;

let parse_game string =
  let hand_strings = String.split (List.last_exn (String.split string ~on:':')) ~on:';' in
  valid_game hand_strings
;;

let parse_minimum_game string =
  let hand_strings = String.split (List.last_exn (String.split string ~on:':')) ~on:';' in
  let max = calc_min_game hand_strings in
  max.green * max.blue * max.red
;;