open Control_flow
open Graphs

let hline = "\n" ^ String.make 80 '-' ^ "\n"

let translate_to_ssa graph =
  Cfg.print_basic_blocks graph;
  print_endline hline;

  Ssa.parameterize_labels graph;
  Cfg.print_basic_blocks graph;
  print_endline hline;

  Ssa.rename_variables graph;
  Cfg.print_basic_blocks graph;
  print_endline hline;

  Ssa.insert_phi_functions graph;
  Cfg.print_basic_blocks graph;
  print_endline hline;

  Ssa.minimize_phi_functions graph;
  Cfg.print_basic_blocks graph;
  print_endline hline;

  graph, Ssa.Graph.create ()

let optimize (graph, ssa_graph) =
  let graph = Optim.optimize graph ssa_graph in
  Cfg.print_basic_blocks graph;
  print_endline hline;
  graph

let translate_out_of_ssa graph =
  let graph = Ssa.destruct graph in
  Cfg.print_basic_blocks graph

let () =
  graph_of_input
    (match Sys.argv with
     | [| _; filename |] -> filename
     | _ -> failwith "Input file required")
  |> translate_to_ssa
  |> optimize
  |> translate_out_of_ssa
