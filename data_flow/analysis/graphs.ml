open Three_address_code__Parse
open Basic
open Basic__Utils
open Control_flow__Cfg

let diamond =
  define_cfg
    ~nodes:
      (1--4)
    ~edges:
      (* Node 5 serves as exit node *)
      [ (1, 2); (1, 3); (2, 4); (3, 4); (4, 5); ]

let fib =
  define_cfg
    ~nodes:
      (1--6)
    ~edges:
      (* Node 7 serves as exit node *)
      [ (1, 2); (1, 6); (2, 3); (3, 4);
        (3, 5); (4, 3); (5, 7); (6, 7); ]

let graph_of_IR ?(input = "control_flow/fib.ir") () =
  parse_file input
  |> basic_blocks
  |> construct_cfg
