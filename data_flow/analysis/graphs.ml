open Three_address_code__Parse
open Basic
open Basic__Utils
open Control_flow

let diamond =
  Cfg.define
    ~nodes:
      (1--4)
    ~edges:
      (* Node 5 serves as exit node *)
      [ (1, 2); (1, 3); (2, 4); (3, 4); (4, 5); ]

let fib =
  Cfg.define
    ~nodes:
      (1--6)
    ~edges:
      (* Node 7 serves as exit node *)
      [ (1, 2); (1, 3); (2, 7); (3, 4);
        (4, 5); (4, 6); (5, 4); (6, 7); ]

let graph_of_IR ?(input = "control_flow/basic_blocks/fib.hir") () =
  parse_file input
  |> basic_blocks
  |> Cfg.construct
