open Control_flow
open Graphs
open Ssa

let () =
  let graph =
    graph_of_input
      (match Sys.argv with
       | [| _; filename |] -> filename
       | _ -> "examples/pow.hir")
  in
  let ssa_graph = convert_to_ssa graph in
  Cfg.print_basic_blocks graph;
  print_newline ();
  Ssa.Graph.output_dot ssa_graph
