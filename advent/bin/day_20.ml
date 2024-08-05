open Core

module FlipFlop = struct
  type record =
    { dest : string list
    ; name : string
    ; signal : int
    }

  let make dest name signal = { dest; name; signal }
  let dest x = x.dest
  let name x = x.name
  let signal x = x.signal

  let update_signal input { dest; name; signal = old } =
    { dest; name; signal = (if input = 0 then old lxor 1 else old) }
  ;;
end

module Conjunction = struct
  type record =
    { name : string
    ; inputs : (string * int) list
    ; dest : string list
    ; iterations : int option
    }

  let make name input dest iterations = { name; inputs = input; dest; iterations }

  let init_input input { name; inputs = i; dest = d; iterations } =
    { name; inputs = i @ [ input, 0 ]; dest = d; iterations }
  ;;

  let get_output { name = _; inputs; dest = _; _ } =
    1 - List.fold inputs ~init:1 ~f:(fun acc (_, x) -> x land acc)
  ;;

  let recieve_signal { name; inputs; dest; iterations } sender_name signal count =
    let inputs =
      List.map inputs ~f:(fun (key, v) ->
        if String.equal key sender_name then key, signal else key, v)
    in
    let signal = 1 - List.fold inputs ~init:1 ~f:(fun acc (_, x) -> x land acc) in
    { name
    ; inputs
    ; dest
    ; iterations = (if signal = 1 then Some (count + 1) else iterations)
    }
  ;;

  let name x = x.name
  let _inputs x = x.inputs
  let dest x = x.dest
  let _iterations x = x.iterations
end

module Broadcaster = struct
  type record = { dest : string list }

  let make dest = { dest }
  let dest x = x.dest
end

module Output = struct
  type record = { tracker : bool }

  let make = { tracker = false }
  let update_signal { tracker } signal = { tracker = tracker || signal = 0 }
  let _tracker x = x.tracker
end

type modules =
  | FlipFlopType of FlipFlop.record
  | ConjunctionType of Conjunction.record
  | BroadcasterType of Broadcaster.record
  | OutputType of Output.record

let conjunction_recieve_signal
  x
  (table : (string, modules) Hashtbl_intf.Hashtbl.t)
  signal
  sender_name
  iter
  =
  let update = Conjunction.recieve_signal x sender_name signal iter in
  let () = Hashtbl.set table ~key:(Conjunction.name x) ~data:(ConjunctionType update) in
  let q = Conjunction.get_output update in
  let () =
    Hashtbl.set table ~key:(Conjunction.name update) ~data:(ConjunctionType update)
  in
  table, List.map (Conjunction.dest update) ~f:(fun d -> d, q, Conjunction.name update)
;;

let flip_flop_recieve_signal x (table : (string, modules) Hashtbl_intf.Hashtbl.t) signal =
  let new_module = FlipFlop.update_signal signal x in
  let output = FlipFlop.signal new_module in
  let () = Hashtbl.set table ~key:(FlipFlop.name x) ~data:(FlipFlopType new_module) in
  ( table
  , if signal = 0
    then List.map (FlipFlop.dest x) ~f:(fun d -> d, output, FlipFlop.name x)
    else [] )
;;

let output_recieve_signal x (table : (string, modules) Hashtbl_intf.Hashtbl.t) signal =
  let new_module = Output.update_signal x signal in
  let () = if signal = 0 then Fmt.pr "here" else () in
  let () = Hashtbl.set table ~key:"rx" ~data:(OutputType new_module) in
  table, []
;;

module Mod = struct
  let get_dest = function
    | FlipFlopType x -> FlipFlop.dest x
    | ConjunctionType x -> Conjunction.dest x
    | BroadcasterType x -> Broadcaster.dest x
    | _ -> []
  ;;

  let visit (table : (string, modules) Hashtbl_intf.Hashtbl.t) signal sender_name iter
    = function
    | FlipFlopType x -> flip_flop_recieve_signal x table signal
    | ConjunctionType x -> conjunction_recieve_signal x table signal sender_name iter
    | BroadcasterType _ -> raise (Invalid_argument "ahh")
    | OutputType x -> output_recieve_signal x table signal
  ;;
end

let get_conj_exn m =
  match m with
  | ConjunctionType m -> m
  | _ -> raise (Invalid_argument "not a conjunction")
;;

let add_module hash_table (name : string) (module_val : modules) =
  match Hashtbl.find hash_table name with
  | None ->
    let () = Hashtbl.add_exn hash_table ~key:name ~data:([], module_val) in
    hash_table
  | Some old_val ->
    (match old_val with
     | inputs, OutputType _ ->
       let () = Hashtbl.set hash_table ~key:name ~data:(inputs, module_val) in
       hash_table
     | _ -> raise (Invalid_argument "duplicate keys"))
;;

let add_input hash_table (name : string) (input : string) =
  match Hashtbl.find hash_table name with
  | None ->
    let () =
      Hashtbl.add_exn hash_table ~key:name ~data:([ input ], OutputType Output.make)
    in
    hash_table
  | Some (inputs, module_val) ->
    let () = Hashtbl.set hash_table ~key:name ~data:(inputs @ [ input ], module_val) in
    hash_table
;;

let rec add_inputs hash_table (dests : string list) (source : string) =
  match dests with
  | [] -> hash_table
  | d :: dests ->
    let hash_table = add_input hash_table d source in
    add_inputs hash_table dests source
;;

let _module_to_string = function
  | FlipFlopType _ -> "%"
  | ConjunctionType _ -> "&"
  | BroadcasterType _ -> ""
  | OutputType _ -> "rx"
;;

let create_module hash_table (name : string) (dests : string list) = function
  | '%' ->
    let hash_table = add_inputs hash_table dests name in
    add_module hash_table name (FlipFlopType (FlipFlop.make dests name 0))
  | '&' ->
    let hash_table = add_inputs hash_table dests name in
    add_module hash_table name (ConjunctionType (Conjunction.make name [] dests None))
  | 'b' ->
    let hash_table = add_inputs hash_table dests name in
    add_module hash_table name (BroadcasterType (Broadcaster.make dests))
  | _ -> raise (Invalid_argument "unrecognized module type")
;;

let parse_line hash_table (line : string) =
  let mod_type, name, dests = Scanf.sscanf line "%c%s@ -> %s@\n" (fun a b c -> a, b, c) in
  create_module
    hash_table
    name
    (List.filter
       (String.split_on_chars dests ~on:[ ','; ' ' ])
       ~f:(fun s -> not (String.equal s "")))
    mod_type
;;

let rec parse_lines hash_table (lines : string list) =
  match lines with
  | [] -> hash_table
  | line :: lines ->
    let hash_table = parse_line hash_table line in
    parse_lines hash_table lines
;;

let build_conjunctions hash_map =
  Hashtbl.map hash_map ~f:(fun (inputs, mod_val) ->
    match mod_val with
    | ConjunctionType conj ->
      ConjunctionType
        (List.fold inputs ~init:conj ~f:(fun i conj -> Conjunction.init_input conj i))
    | _ -> mod_val)
;;

let list_to_string list = List.fold list ~init:"" ~f:(fun i acc -> acc ^ ", " ^ i)

let print_table_value name mod_val =
  let mod_type = _module_to_string mod_val in
  let dests = list_to_string (Mod.get_dest mod_val) in
  Fmt.pr "%s%s -> %s\n\n" mod_type name dests
;;

(*simulation functions*)

let visit (table : (string, modules) Hashtbl_intf.Hashtbl.t) queue low high iter =
  match queue with
  | [] -> table, low, high, queue
  | (name, signal, sender_name) :: queue ->
    let node = Hashtbl.find_exn table name in
    let table, q = Mod.visit table signal sender_name iter node in
    ( table
    , (if signal = 0 then low + 1 else low)
    , (if signal = 0 then high else high + 1)
    , queue @ q )
;;

let rec run_iteration_inner
  (table : (string, modules) Hashtbl_intf.Hashtbl.t)
  low
  high
  iter
  = function
  | [] -> table, low, high
  | queue ->
    let table, low, high, queue = visit table queue low high iter in
    run_iteration_inner table low high iter queue
;;

let run_iteration (table : (string, modules) Hashtbl_intf.Hashtbl.t) low high iter =
  let queue = Mod.get_dest (Hashtbl.find_exn table "roadcaster") in
  let queue = List.map queue ~f:(fun x -> x, 0, "roadcaster") in
  run_iteration_inner table (low + 1) high iter queue
;;

let rec simulate (table : (string, modules) Hashtbl_intf.Hashtbl.t) low high = function
  | 0 -> table, low, high
  | i ->
    let table, low, high = run_iteration table low high (1000 - i) in
    simulate table low high (i - 1)
;;

let check_for_answer (table : (string, modules) Hashtbl_intf.Hashtbl.t) =
  let sb = Conjunction._iterations (get_conj_exn (Hashtbl.find_exn table "sb")) in
  let nd = Conjunction._iterations (get_conj_exn (Hashtbl.find_exn table "nd")) in
  let ds = Conjunction._iterations (get_conj_exn (Hashtbl.find_exn table "ds")) in
  let hf = Conjunction._iterations (get_conj_exn (Hashtbl.find_exn table "hf")) in
  match sb, nd, ds, hf with
  | Some a, Some b, Some c, Some d -> Some (a * b * c * d)
  | _ -> None
;;

let rec simulate_p2 (table : (string, modules) Hashtbl_intf.Hashtbl.t) acc limit =
  let table, _, _ = run_iteration table 0 0 acc in
  match check_for_answer table with
  | Some r -> r
  | None -> simulate_p2 table (acc + 1) limit
;;

(* main function below*)

let table =
  Advent.Advent_tools.read_lines "./input/puzzle20.txt"
  |> parse_lines (Hashtbl.create ~size:10000 ~growth_allowed:true (module String))
  |> build_conjunctions
;;

let table_clone = Hashtbl.copy table
let () = Hashtbl.iteri table ~f:(fun ~key ~data:mod_val -> print_table_value key mod_val)
let _, low_pulses, high_pulses = simulate table 0 0 1000

let () =
  Fmt.pr "low: %d high: %d -> %d\n" low_pulses high_pulses (low_pulses * high_pulses)
;;

let result = simulate_p2 table_clone 0 5000
let () = Fmt.pr "part 2: %d" result
