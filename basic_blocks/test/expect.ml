open Three_address_code__IR
open Three_address_code__Parse
open Basic_block
open Basic_block__Utils

let string_of_vars vars =
  List.map (fun (Var x) -> x) vars
  |> String.concat ", "

let print_use_def blocks =
  let rows = List.(rev (fold_left (fun rows block ->
      let use, def = Liveness.compute block in
      [ block.name;
        string_of_vars (Liveness.Set.elements use);
        string_of_vars (Liveness.Set.elements def);
      ] :: rows
    ) [] blocks))
  in
  print_table ~rows:([""; "use"; "def"] :: rows)

let () =
  match Sys.argv with
  | [| _; filename |] ->
    let blocks =
      snd @@ parse_file filename
      |> create_basic_blocks
    in
    print_basic_blocks blocks;
    print_newline ();
    print_use_def blocks
  | _ ->
    failwith "Input file required"
