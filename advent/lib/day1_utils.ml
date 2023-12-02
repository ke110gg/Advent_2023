open Core

let target_values =
  [ "one", '1'
  ; "two", '2'
  ; "three", '3'
  ; "four", '4'
  ; "five", '5'
  ; "six", '6'
  ; "seven", '7'
  ; "eight", '8'
  ; "nine", '9'
  ]
;;

let find_num pos str =
  let first_char = String.get str pos in
  if Char.is_digit first_char
  then Some first_char
  else
    List.find_map target_values ~f:(fun (substr, value) ->
      match String.is_substring_at str ~pos ~substring:substr with
      | true -> Some value
      | false -> None)
;;

let rec find_first_num pos iter string =
  if pos >= String.length string
  then None
  else (
    match find_num pos string with
    | Some x -> Some x
    | None -> find_first_num (pos + iter) iter string)
;;
