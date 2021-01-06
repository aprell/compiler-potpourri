open Three_address_code__Parse
open Three_address_code__IR

let () =
  parse_file
    (match Sys.argv with
     | [| _; filename |] -> filename
     | _ -> "examples/test01.hir")
  |> dump
