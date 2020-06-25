open Three_address_code__IR
open Three_address_code__Parse
open Basic_block
open Basic_block__Utils

let print_basic_blocks blocks =
  List.map to_string blocks
  |> String.concat "\n"
  |> print_endline

let string_of_vars vars =
  List.map (fun (Var x) -> x) vars
  |> String.concat ", "

let print_use_def blocks =
  let column_widths = [3; 20; 20] in
  let rows = List.(rev (fold_left (fun rows { name; source } ->
      match source with
      | Some { use; def; _ } ->
        [name; string_of_vars use; string_of_vars def] :: rows
      | None ->
        failwith "print_use_def"
    ) [] blocks))
  in
  print_table
    ~column_widths
    ~rows:([""; "use"; "def"] :: rows)

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
