open Control_flow
open Graphs

let print_cfg = Cfg.output_dot

let print_domtree graph =
  let open Dom in
  let _ = dominators graph in
  let _ = immediate_dominators graph in
  Domtree.(output_dot (create graph))

let print_loop_nesting graph =
  Loop.NestingForest.(output_dot (create graph))

let () =
  match Sys.argv with
  | [| _; filename |] ->
    let graph = graph_of_input filename in
    print_cfg graph;
    print_newline ();
    print_domtree graph;
    print_newline ();
    print_loop_nesting graph
  | _ ->
    failwith "Input file required"
