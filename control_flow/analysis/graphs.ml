open Three_address_code__Parse
open Basic_block
open Basic_block__Utils

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

let loops = [
  Cfg.define
    ~nodes:
      (1--7)
    ~edges:
      (* Node 8 serves as exit node *)
      [ (1, 2); (1, 3); (2, 3); (3, 4);
        (4, 3); (4, 5); (5, 6); (5, 7);
        (6, 3); (6, 4); (7, 8); ];

  Cfg.define
    ~nodes:
      (1--8)
    ~edges:
      (* Node 9 serves as exit node *)
      [ (1, 2); (2, 3); (3, 4); (4, 5);
        (5, 4); (5, 6); (6, 3); (6, 7);
        (7, 2); (7, 8); (8, 9); ]
]

let graph_of_input filename =
  parse_file filename
  |> create_basic_blocks
  |> Cfg.construct
