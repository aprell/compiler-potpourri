open Parse
open Basic

let print_basic_blocks filename =
  parse_file filename
  |> basic_blocks
  |> List.map to_string
  |> String.concat "\n"
  |> print_endline

let () =
  print_basic_blocks
    (match Sys.argv with
     | [| _; filename |] -> filename
     | _ -> "fib.ir")
