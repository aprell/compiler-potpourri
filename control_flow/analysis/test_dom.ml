open Basic
open Cfg
open Dom
open Parse

let fib_cfg_1 =
  define_cfg
    ~nodes:
      Utils.(1--6)
    ~edges:
      (* Node 7 serves as exit node *)
      [ (1, 2); (1, 6); (2, 3); (3, 4);
        (3, 5); (4, 3); (5, 7); (6, 7); ]

let fib_cfg_2 =
  parse_file "fib.ir"
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
