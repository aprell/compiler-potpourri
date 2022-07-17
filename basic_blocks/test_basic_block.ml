open Three_address_code__Parse
open Basic_block

let print_basic_blocks filename =
  snd @@ parse_file filename
  |> create_basic_blocks
  |> print_basic_blocks

let () =
  print_basic_blocks
    (match Sys.argv with
     | [| _; filename |] -> filename
     | _ -> "examples/fib.hir")
