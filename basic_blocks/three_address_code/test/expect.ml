open Three_address_code__Parse
open Three_address_code__IR

let () =
  snd @@ parse_file
    (match Sys.argv with
     | [| _; filename |] -> filename
     | _ -> failwith "Input file required")
  |> dump
