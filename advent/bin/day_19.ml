open Core

type workflow_result =
  | Accepted
  | Rejected
  | Function of (int * int * int * int -> workflow_result)
  | Continue of string

module StringMap = Map.Make (String)

let parse_result_type = function
  | "A" -> Accepted
  | "R" -> Rejected
  | s -> Continue s
;;

let greater_than rhs lhs = lhs > rhs
let less_than rhs lhs = lhs < rhs

let get_operator_fun = function
  | '>' -> greater_than
  | '<' -> less_than
  | _ -> raise (Invalid_argument "")
;;

let get_cat_input (x, m, a, s) = function
  | 'x' -> x
  | 'm' -> m
  | 'a' -> a
  | 's' -> s
  | _ -> raise (Invalid_argument "")
;;

let lambda category operator true_dest false_dest full_input =
  let input = get_cat_input full_input category in
  if operator input
  then parse_result_type true_dest
  else (
    match false_dest with
    | Accepted -> Accepted
    | Rejected -> Rejected
    | Function f -> f full_input
    | Continue s -> Continue s)
;;

let rec parse_rule rule =
  match rule with
  | "R" -> Rejected
  | "A" -> Accepted
  | rule ->
    if not (String.mem rule ',')
    then Continue rule
    else (
      let category, operator, threshold, true_dest, false_dest =
        Scanf.sscanf
          rule
          "%c%c %s@:%s@,%s"
          (fun input_name operator threshold true_dest false_dest ->
             input_name, operator, threshold, true_dest, false_dest)
      in
      let () =
        Fmt.pr "%c, %c, %s, %s, %s \n" category operator threshold true_dest false_dest
      in
      let operator = (get_operator_fun operator) (Int.of_string threshold) in
      let next = parse_rule false_dest in
      Function (lambda category operator true_dest next))
;;

let rec parse_rule_inputs map = function
  | [] -> raise (Invalid_argument "should reach end of list")
  | "" :: lines -> map, lines
  | line :: lines ->
    let dest, rule_string = Scanf.sscanf line "%s@{%s@}" (fun a b -> a, b) in
    let rule = parse_rule rule_string in
    let map = StringMap.add_exn map ~key:dest ~data:rule in
    parse_rule_inputs map lines
;;

let rec parse_inputs output = function
  | [] -> output
  | input :: lines ->
    let output =
      output
      @ [ Scanf.sscanf input "{x=%s@,m=%s@,a=%s@,s=%s@}" (fun x m a s ->
            Int.of_string x, Int.of_string m, Int.of_string a, Int.of_string s)
        ]
    in
    parse_inputs output lines
;;

let sum_input (a, b, c, d) = a + b + c + d

let rec eval_rule_on_input rules rule input =
  let lam = StringMap.find_exn rules rule in
  let result =
    match lam with
    | Function f -> f input
    | Accepted -> Accepted
    | Rejected -> Rejected
    | _ -> raise (Invalid_argument "")
  in
  match result with
  | Accepted -> sum_input input
  | Rejected -> 0
  | Continue s -> eval_rule_on_input rules s input
  | _ -> raise (Invalid_argument "")
;;

let rec evaluate_inputs rules output = function
  | [] -> output
  | input :: inputs ->
    let result = eval_rule_on_input rules "in" input in
    let output = output + result in
    evaluate_inputs rules output inputs
;;

let lines = Advent.Advent_tools.read_lines "./input/puzzle19.txt"
let rules, lines = parse_rule_inputs StringMap.empty lines
let inputs = parse_inputs [] lines
let result = evaluate_inputs rules 0 inputs
let () = Fmt.pr "Result: %d\n" result
