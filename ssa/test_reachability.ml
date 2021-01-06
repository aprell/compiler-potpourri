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
(*   Ssa__Optim.optimize (); *)
  Cfg.print_basic_blocks graph;
  let worklist = Reachability.init graph in
  print_newline ();
  Reachability.print ();
  print_newline ();
  Reachability.iterate worklist;
  print_newline ();
  Reachability.print ()
