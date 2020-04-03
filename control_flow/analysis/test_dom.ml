open Three_address_code__Parse
open Basic
open Control_flow__Cfg
open Control_flow__Dom
open Control_flow__Inspect

let fib_cfg_1 =
  define_cfg
    ~nodes:
      Basic__Utils.(1--6)
    ~edges:
      (* Node 7 serves as exit node *)
      [ (1, 2); (1, 3); (2, 7); (3, 4);
        (4, 5); (4, 6); (5, 4); (6, 7); ]

let fib_cfg_2 =
  parse_file "basic_blocks/fib.hir"
  |> basic_blocks
  |> construct_cfg

let inspect_control_flow graph =
  let dom_sets = dominators graph in
  let idoms = immediate_dominators graph dom_sets in
  let back_edges = back_edges graph dom_sets in
  inspect graph ~dom_sets ~idoms ~back_edges

let () =
  inspect_control_flow fib_cfg_1;
  (* Prints the same information *)
  inspect_control_flow fib_cfg_2
