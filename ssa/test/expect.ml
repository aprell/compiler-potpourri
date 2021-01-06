open Control_flow
open Graphs
open Ssa

let convert_to_ssa graph =
  let hline = "\n" ^ String.make 80 '-' ^ "\n" in
  Cfg.print_basic_blocks graph;
  print_endline hline;

  parameterize_labels graph;
  Cfg.print_basic_blocks graph;
  print_endline hline;

  rename_variables graph;
  Cfg.print_basic_blocks graph;
  print_endline hline;

  insert_phi_functions graph;
  Cfg.print_basic_blocks graph;
  print_endline hline;

  minimize_phi_functions graph;
  Cfg.print_basic_blocks graph;
  print_endline hline;

  Ssa__Optim.optimize ();
  Cfg.print_basic_blocks graph

let () =
  graph_of_input
    (match Sys.argv with
     | [| _; filename |] -> filename
     | _ -> failwith "Input file required")
  |> convert_to_ssa
