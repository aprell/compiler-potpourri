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
  let _ = convert_to_ssa graph in
  Cfg.print_basic_blocks graph;
  let worklist = Reachability.init graph ~verbose:true in
  print_newline ();
  Reachability.print ();
  print_newline ();
  Reachability.iterate worklist;
  print_newline ();
  Reachability.print ()
