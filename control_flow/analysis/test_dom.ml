open Three_address_code__Parse
open Basic
open Control_flow__Cfg
open Control_flow__Dom
open Control_flow__Inspect

let graph_of_input filename =
  parse_file filename
  |> basic_blocks
  |> construct_cfg

let inspect_control_flow graph =
  let _ = dominators graph in
  let _ = immediate_dominators graph in
  inspect graph

let test () =
  let fib_cfg_1 =
    define_cfg
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
