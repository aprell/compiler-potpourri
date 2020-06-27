open Control_flow__Graphs
open Dominators
open Liveness

let () =
  match Sys.argv with
  | [| _; filename |] ->
    let graph = graph_of_input filename in
    print_endline "LIVE VARIABLES\n";
    ignore (Liveness.compute graph ~dump:true);
    print_endline "DOMINATORS\n";
    ignore (Dominators.compute graph ~dump:true)
  | _ ->
    failwith "Input file required"
