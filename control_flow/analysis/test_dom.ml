open Control_flow
open Graphs

let inspect graph =
  let open Dom in
  let _ = dominators graph in
  let _ = immediate_dominators graph in
  Cfg.inspect graph ~back_edges:(back_edges graph)

let test () =
  let fib_1 = Graphs.fib in
  let fib_2 = graph_of_input "basic_blocks/fib.hir" in
  inspect fib_1;
  (* Prints the same information *)
  inspect fib_2

let () =
  match Sys.argv with
  | [| _; filename |] ->
    graph_of_input filename
    |> inspect
  | _ ->
    test ()
