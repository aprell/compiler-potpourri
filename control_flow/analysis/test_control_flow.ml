open Control_flow
open Graphs

let inspect graph ~output =
  let open Dom in
  let _ = dominators graph in
  let _ = immediate_dominators graph in
  Cfg.inspect graph ~back_edges:(back_edges graph);
  Cfg.output_dot graph ~filename:(output ^ "_cfg.dot");
  Domtree.(output_dot (create graph) ~filename:(output ^ "_domtree.dot"))

let test () =
  let fib_1 = Graphs.fib in
  let fib_2 = graph_of_input "basic_blocks/examples/fib.hir" in
  (* fib_1 lacks source information *)
  assert (not (Cfg.equal fib_1 fib_2));
  assert (Cfg.equal fib_1 (Cfg.discard_source_info fib_2))

let () =
  match Sys.argv with
  | [| _; filename |] ->
    graph_of_input filename
    |> inspect ~output:
      Filename.("examples/" ^ (remove_extension (basename filename)))
  | _ ->
    test ()