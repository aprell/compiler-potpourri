open Control_flow
open Graphs

let print_loops graph =
  let indent = String.make 4 ' ' in
  match Loop.back_edges graph with
  | _ :: _ as edges ->
    print_endline "\nLoops:";
    List.iter (fun edge ->
        print_string indent;
        Loop.(print (find edge))
      ) edges
  | [] -> ()

let inspect graph ~output =
  let open Dom in
  let _ = dominators graph in
  let _ = immediate_dominators graph in
  Cfg.inspect graph;
  Cfg.output_dot graph ~filename:(output ^ "_cfg.dot");
  Domtree.(output_dot (create graph) ~filename:(output ^ "_domtree.dot"));
  print_loops graph

let test () =
  let fib_1 = Graphs.fib in
  let fib_2 = graph_of_input "basic_blocks/examples/fib.hir" in
  assert (Cfg.equal fib_1 fib_2)

let () =
  match Sys.argv with
  | [| _; filename |] ->
    graph_of_input filename
    |> inspect ~output:
      Filename.("examples/" ^ (remove_extension (basename filename)))
  | _ ->
    test ()
