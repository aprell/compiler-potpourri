open Control_flow
open Graphs

let translate_to_ssa graph =
  let hline = "\n" ^ String.make 80 '-' ^ "\n" in
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

  let ssa_graph = Ssa.Graph.create () in
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
  |> translate_out_of_ssa
