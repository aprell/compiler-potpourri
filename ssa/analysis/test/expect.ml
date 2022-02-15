open Control_flow
open Graphs
open Analysis

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

  (* Sparse simple constant propagation *)
  print_endline "Result of SSCP:";
  Sscp.(run ssa_graph |> print);
  print_newline ();

  (* Sparse conditional constant propagation *)
  print_endline "Result of SCCP:";
  Sccp.(run graph ssa_graph |> print)
