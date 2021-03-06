open Control_flow
open Graphs

let () =
  let graph =
    graph_of_input
      (match Sys.argv with
       | [| _; filename |] -> filename
       | _ -> "examples/pow.hir")
  in
  let ssa_graph = Ssa.construct graph in
  Cfg.print_basic_blocks graph;
  print_newline ();
  Ssa.Graph.print ssa_graph;
  print_newline ();
  let graph = Optim.optimize graph ssa_graph ~dump:true in
  Cfg.print_basic_blocks graph
