open Control_flow__Graphs
open Liveness

let () =
  match Sys.argv with
  | [| _; filename |] ->
    graph_of_input filename
    |> Liveness.compute ~dump:true
    |> ignore
  | _ ->
    failwith "Input file required"
