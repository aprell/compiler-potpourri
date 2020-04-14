open Three_address_code__Parse
open Basic
open Control_flow
open Control_flow__Inspect

let graph_of_input filename =
  parse_file filename
  |> basic_blocks
  |> Cfg.construct

let test () =
  let fib_cfg_1 =
    Cfg.define
      ~nodes:
        Basic__Utils.(1--6)
      ~edges:
        (* Node 7 serves as exit node *)
        [ (1, 2); (1, 3); (2, 7); (3, 4);
          (4, 5); (4, 6); (5, 4); (6, 7); ]
  in
  let fib_cfg_2 = graph_of_input "basic_blocks/fib.hir" in
  inspect fib_cfg_1;
  Cfg.output_dot fib_cfg_2 ~filename:"fib.dot";
  (* fib_cfg_1 lacks source information *)
  assert (not (Cfg.equal fib_cfg_1 fib_cfg_2));
  assert (Cfg.equal fib_cfg_1 (Cfg.discard_source_info fib_cfg_2))

let () =
  match Sys.argv with
  | [| _; filename |] ->
    let cfg = graph_of_input filename in
    inspect cfg;
    Cfg.output_dot cfg
      ~filename:Filename.((remove_extension (basename filename)) ^ ".dot")
  | _ ->
    test ()
