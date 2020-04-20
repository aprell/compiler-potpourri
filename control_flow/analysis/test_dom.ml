open Control_flow
open Graphs

let inspect ?domtree graph =
  let open Dom in
  let _ = dominators graph in
  let _ = immediate_dominators graph in
  Cfg.inspect graph ~back_edges:(back_edges graph);
  match domtree with
  | Some filename -> Domtree.output_dot (Domtree.create graph) ~filename
  | None -> ()

let test () =
  let fib_1 = Graphs.fib in
  let fib_2 = graph_of_input "basic_blocks/fib.hir" in
  inspect fib_1;
  (* Prints the same information *)
  inspect fib_2 ~domtree:"fib_domtree.dot"

let () =
  match Sys.argv with
  | [| _; filename |] ->
    graph_of_input filename
    |> inspect ~domtree:
      Filename.((remove_extension (basename filename)) ^ "_domtree.dot")
  | _ ->
    test ()
