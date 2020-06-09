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
  parameterize_labels graph;
  rename_variables graph;
  insert_phi_functions graph;
  minimize_phi_functions graph;
  print_basic_blocks graph

let () =
  graph_of_input
    (match Sys.argv with
     | [| _; filename |] -> filename
     | _ -> "examples/pow.hir")
  |> convert_to_ssa
