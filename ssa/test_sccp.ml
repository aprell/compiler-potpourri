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
  let worklist = Sccp.init graph ssa_graph ~verbose:true in
  print_newline ();
  Sccp.print ();
  print_newline ();
  Sccp.iterate worklist ssa_graph;
  print_newline ();
  Sccp.print ()
