open Control_flow
open Graphs
open Analysis

(* Sparse simple constant propagation *)
let sscp ssa_graph =
  let worklist = Sscp.init ssa_graph ~value:Sscp.Top in
  Sscp.iterate worklist ssa_graph;
  print_endline "Result of SSCP:";
  Sscp.print ()

(* Sparse conditional constant propagation *)
let sccp graph ssa_graph =
  let worklist = Sccp.init graph ssa_graph in
  Sccp.iterate worklist ssa_graph;
  print_endline "Result of SCCP:";
  Sccp.print ()

let () =
  let graph =
    graph_of_input
      (match Sys.argv with
       | [| _; filename |] -> filename
       | _ -> failwith "Input file required")
  in
  let ssa_graph = Ssa.construct graph in
  Cfg.print_basic_blocks graph;
  print_newline ();
  sscp ssa_graph;
  print_newline ();
  sccp graph ssa_graph
