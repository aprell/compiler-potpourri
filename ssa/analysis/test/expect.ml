open Control_flow
open Graphs
open Ssa

(* Sparse simple constant propagation *)
let sscp _graph =
  let worklist = Sscp.init ~value:Sscp.Top () in
  Sscp.iterate worklist;
  print_endline "Result of SSCP:";
  Sscp.print ()

(* Sparse conditional constant propagation *)
let sccp graph =
  let worklist = Sccp.init graph in
  Sccp.iterate worklist;
  print_endline "Result of SCCP:";
  Sccp.print ()

let () =
  let graph =
    graph_of_input
      (match Sys.argv with
       | [| _; filename |] -> filename
       | _ -> failwith "Input file required")
  in
  convert_to_ssa graph;
  Cfg.print_basic_blocks graph;
  print_newline ();
  sscp graph;
  print_newline ();
  sccp graph
