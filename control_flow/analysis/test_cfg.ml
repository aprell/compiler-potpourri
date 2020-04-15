open Control_flow
open Graphs

let test () =
  let fib_1 = Graphs.fib in
  let fib_2 = graph_of_input "basic_blocks/fib.hir" in
  Cfg.inspect fib_1;
  Cfg.output_dot fib_2 ~filename:"fib.dot";
  (* fib_1 lacks source information *)
  assert (not (Cfg.equal fib_1 fib_2));
  assert (Cfg.equal fib_1 (Cfg.discard_source_info fib_2))

let () =
  match Sys.argv with
  | [| _; filename |] ->
    let cfg = graph_of_input filename in
    Cfg.inspect cfg;
    Cfg.output_dot cfg
      ~filename:Filename.((remove_extension (basename filename)) ^ ".dot")
  | _ ->
    test ()
