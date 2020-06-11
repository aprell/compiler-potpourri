open Control_flow
open Graphs
open Ssa

let print_basic_blocks graph =
  Cfg.basic_blocks graph
  |> List.map Basic_block.to_string
  |> List.filter (fun name -> name <> "Entry" && name <> "Exit")
  |> String.concat "\n"
  |> print_endline

let convert_to_ssa graph =
  let hline = "\n" ^ String.make 80 '-' ^ "\n" in
  print_basic_blocks graph;
  print_endline hline;

  parameterize_labels graph;
  print_basic_blocks graph;
  print_endline hline;

  rename_variables graph;
  print_basic_blocks graph;
  print_endline hline;

  insert_phi_functions graph;
  print_basic_blocks graph;
  print_endline hline;

  minimize_phi_functions graph;
  print_basic_blocks graph

let () =
  graph_of_input
    (match Sys.argv with
     | [| _; filename |] -> filename
     | _ -> failwith "Input file required")
  |> convert_to_ssa
