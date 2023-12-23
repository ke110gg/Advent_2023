open Core

type pipe =
  | VERTICAL
  | HORIZONTAL
  | L_PIPE
  | J_PIPE
  | SEV_PIPE
  | F_PIPE
  | GROUND
  | START

let char_to_pipe char =
  match char with
  | '|' -> VERTICAL
  | '-' -> HORIZONTAL
  | 'L' -> L_PIPE
  | 'J' -> J_PIPE
  | '7' -> SEV_PIPE
  | 'F' -> F_PIPE
  | '.' -> GROUND
  | 'S' -> START
  | c -> raise (Invalid_argument (Fmt.str "invalid pipe %c" c))
;;

let accepts_movement p delta =
  match delta with
  | 0, 1 ->
    (match p with
     | J_PIPE -> true
     | HORIZONTAL -> true
     | SEV_PIPE -> true
     | _ -> false)
  | 1, 0 ->
    (match p with
     | VERTICAL -> true
     | J_PIPE -> true
     | L_PIPE -> true
     | _ -> false)
  | -1, 0 ->
    (match p with
     | VERTICAL -> true
     | F_PIPE -> true
     | SEV_PIPE -> true
     | _ -> false)
  | 0, -1 ->
    (match p with
     | HORIZONTAL -> true
     | F_PIPE -> true
     | L_PIPE -> true
     | _ -> false)
  | _ -> raise (Invalid_argument "invalid movement")
;;

let rec parse_line line new_line =
  match line with
  | [] -> new_line
  | c :: body ->
    let ch = char_to_pipe c in
    parse_line body (new_line @ [ ch ])
;;

let rec build_grid lines new_grid =
  match lines with
  | [] -> new_grid
  | line :: body ->
    let new_line = parse_line (String.to_list line) [] in
    build_grid body (new_grid @ [ new_line ])
;;

let rec find_start_col row acc =
  match row with
  | [] -> None
  | a :: body ->
    (match a with
     | START -> Some acc
     | _ -> find_start_col body (acc + 1))
;;

let rec find_start grid acc =
  match grid with
  | [] -> None
  | current :: body ->
    (match find_start_col current 0 with
     | Some y -> Some (acc, y)
     | None -> find_start body (acc + 1))
;;

let next_point grid (current_y, current_x) (prev_y, prev_x) =
  if current_y < 0
     || current_y >= List.length grid
     || current_x < 0
     || current_x >= List.length (List.hd_exn grid)
  then None
  else (
    let delta_y = current_y - prev_y in
    let dela_x = current_x - prev_x in
    match List.nth_exn (List.nth_exn grid current_y) current_x with
    | GROUND -> None
    | START -> raise (Invalid_argument "somehow ended up at start")
    | VERTICAL ->
      Some (if delta_y < 0 then current_y - 1, current_x else current_y + 1, current_x)
    | L_PIPE ->
      Some (if delta_y > 0 then current_y, current_x + 1 else current_y - 1, current_x)
    | F_PIPE ->
      Some (if delta_y < 0 then current_y, current_x + 1 else current_y + 1, current_x)
    | J_PIPE ->
      Some (if delta_y > 0 then current_y, current_x - 1 else current_y - 1, current_x)
    | SEV_PIPE ->
      Some (if delta_y < 0 then current_y, current_x - 1 else current_y + 1, current_x)
    | HORIZONTAL ->
      Some (if dela_x > 0 then current_y, current_x + 1 else current_y, current_x - 1))
;;

let extend_path path grid =
  match path with
  | current :: prev :: _ ->
    (match next_point grid current prev with
     | None -> None
     | Some x -> Some ([ x ] @ path))
  | _ -> raise (Invalid_argument "extend_path error")
;;

let rec find_intersections list =
  match list with
  | [] -> None
  | _ :: [] -> None
  | a :: body ->
    if List.mem body ~equal:(fun (x1, y1) (x2, y2) -> x1 = x2 && y1 = y2) a
    then Some a
    else find_intersections body
;;

let rec make_paths grid paths =
  let new_paths =
    List.fold paths ~init:[] ~f:(fun acc l ->
      match extend_path l grid with
      | None -> acc
      | Some x -> List.append acc [ x ])
  in
  if List.length new_paths = 0
  then raise (Invalid_argument "no more paths")
  else (
    let current_points = List.map new_paths ~f:(fun x -> List.hd_exn x) in
    let prev_points = List.map paths ~f:(fun x -> List.hd_exn x) in
    let () = List.iter prev_points ~f:(fun (y, x) -> Fmt.pr "(%d %d)," y x) in
    let () = Fmt.pr "\n" in
    let () =
      List.iter current_points ~f:(fun (current_y, current_x) ->
        Fmt.pr "(%d %d)," current_y current_x)
    in
    let () = Fmt.pr "\n\n" in
    let intersection = find_intersections (current_points @ prev_points) in
    match intersection with
    | Some _ -> List.length (List.hd_exn new_paths) - 1
    | None -> make_paths grid new_paths)
;;

let rec build_start grid movement (start_y, start_x) acc =
  match movement with
  | [] -> acc
  | (movement_y, movement_x) :: movement ->
    let new_y, new_x = start_y + movement_y, start_x + movement_x in
    let invalid_path =
      new_y < 0
      || new_x < 0
      || new_y >= List.length grid
      || new_x >= List.length (List.hd_exn grid)
    in
    let acc =
      if (not invalid_path)
         && accepts_movement
              (List.nth_exn (List.nth_exn grid new_y) new_x)
              (movement_y, movement_x)
      then acc @ [ [ new_y, new_x; start_y, start_y ] ]
      else acc
    in
    build_start grid movement (start_y, start_x) acc
;;

let lines = Advent.Advent_tools.read_lines "./input/puzzle10.txt"
let grid = build_grid lines []

let start_x, start_y =
  match find_start grid 0 with
  | None -> raise (Invalid_argument "no start")
  | Some x -> x
;;

let start = start_x, start_y
let () = Fmt.pr "start %d %d \n" start_x start_y
let movement = [ 1, 0; -1, 0; 0, -1; 0, 1 ]
let start_paths = build_start grid movement start []
let () = make_paths grid start_paths |> Fmt.pr "Result: %d\n"
