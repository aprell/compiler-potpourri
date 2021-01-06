open Control_flow
open Graphs
open Ssa
open Ssa__Optim

let () =
  let graph =
    graph_of_input
      (match Sys.argv with
       | [| _; filename |] -> filename
       | _ -> "examples/pow.hir")
  in
  convert_to_ssa graph;
  Cfg.print_basic_blocks graph;
  print_newline ();
  optimize ~dump:true ();
  Cfg.print_basic_blocks graph
