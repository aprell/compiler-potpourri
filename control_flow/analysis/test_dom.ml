open Three_address_code__Parse
open Basic
open Control_flow

let graph_of_input filename =
  parse_file filename
  |> basic_blocks
  |> Cfg.construct

let inspect_control_flow graph =
  let open Dom in
  let _ = dominators graph in
  let _ = immediate_dominators graph in
  Cfg.inspect graph ~back_edges:(back_edges graph)

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
  inspect_control_flow fib_cfg_1;
  (* Prints the same information *)
  inspect_control_flow fib_cfg_2

let () =
  match Sys.argv with
  | [| _; filename |] ->
    graph_of_input filename
    |> inspect_control_flow
  | _ ->
    test ()
