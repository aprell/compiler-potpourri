open Three_address_code__IR
open Three_address_code__Parse
open Basic_block

let print_basic_blocks blocks =
  List.map to_string blocks
  |> String.concat "\n"
  |> print_endline

let pad str len =
  str ^ String.make (len - String.length str) ' '

let string_of_vars vars =
  List.map (fun (Var x) -> x) vars
  |> String.concat ", "

let print_use_def blocks =
  let column_widths = [3; 20; 20] in
  let hline = List.map (Fun.flip String.make '-') column_widths in
  let print_hline () =
    hline
    |> String.concat "-+-"
    |> Printf.sprintf "+-%s-+"
    |> print_endline
  in
  let print_row row =
    List.map2 pad row column_widths
    |> String.concat " | "
    |> Printf.sprintf "| %s |"
    |> print_endline
  in
  print_hline ();
  print_row [""; "use"; "def"];
  print_hline ();
  List.iter (fun { name; source } ->
      match source with
      | Some { use; def; _ } ->
        print_row [name; string_of_vars use; string_of_vars def]
      | None ->
        failwith "print_use_def"
    ) blocks;
  print_hline ()

let () =
  match Sys.argv with
  | [| _; filename |] ->
    let blocks =
      parse_file filename
      |> create_basic_blocks
    in
    print_basic_blocks blocks;
    print_newline ();
    print_use_def blocks
  | _ ->
    failwith "Input file required"
