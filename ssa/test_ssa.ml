open Control_flow
open Graphs
open Ssa
open Optim

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
  let graph = optimize graph ~dump:true in
  Cfg.print_basic_blocks graph
