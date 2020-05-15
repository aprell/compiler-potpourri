open Three_address_code__Parse
open Basic

let print_basic_blocks filename =
  parse_file filename
  |> create_basic_blocks
  |> List.map to_string
  |> String.concat "\n"
  |> print_endline

let () =
  print_basic_blocks
    (match Sys.argv with
     | [| _; filename |] -> filename
     | _ -> "examples/fib.hir")
