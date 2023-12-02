open Core

let read_lines file =
  Stdio.In_channel.with_file file ~f:(fun ch ->
    let x = In_channel.input_all ch in
    String.split_lines x)
;;
