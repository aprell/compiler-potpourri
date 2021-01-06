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
  let worklist = Sscp.init ~value:Sscp.Top ~verbose:true () in
  print_newline ();
  Sscp.print ();
  print_newline ();
  Sscp.iterate worklist;
  print_newline ();
  Sscp.print ()
