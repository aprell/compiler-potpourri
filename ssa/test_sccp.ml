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
  convert_to_ssa graph;
  Cfg.print_basic_blocks graph;
  let worklist = Sccp.init graph ~verbose:true in
  print_newline ();
  Sccp.print ();
  print_newline ();
  Sccp.iterate worklist;
  print_newline ();
  Sccp.print ()
