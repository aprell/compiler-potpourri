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
  let worklist = Sscp.init ssa_graph ~value:Sscp.Top ~verbose:true in
  print_newline ();
  Sscp.print ();
  print_newline ();
  Sscp.iterate worklist ssa_graph;
  print_newline ();
  Sscp.print ()
