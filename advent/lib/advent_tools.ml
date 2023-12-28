open Core

let read_line file =
  Stdio.In_channel.with_file file ~f:(fun ch -> In_channel.input_all ch)
;;

let read_lines file = String.split_lines (read_line file)
