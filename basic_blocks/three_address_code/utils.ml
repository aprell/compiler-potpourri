let input_lines chan =
  let rec loop lines =
    match input_line chan with
    | line -> loop (line :: lines)
    | exception End_of_file -> List.rev lines
  in
  loop []

let read_file name =
  let file = open_in name in
  let lines = input_lines file in
  close_in file;
  lines

let read_file_into_string name =
  let file = open_in name in
  let len = in_channel_length file in
  let lines = really_input_string file len in
  close_in file;
  lines

let printf ?(indent = 0) =
  print_string (String.make indent ' ');
  Printf.printf

let sprintf ?(indent = 0) =
  let indent = String.make indent ' ' in
  Printf.ksprintf (( ^ ) indent)

let gen_number init =
  let count = ref init in
  fun () ->
    let c = !count in
    incr count; c

let gen_name pref init =
  let next_number = gen_number init in
  fun ?(pref = pref) () ->
    let c = next_number () in
    pref ^ string_of_int c

let ( >> ) f g x = g (f x)
