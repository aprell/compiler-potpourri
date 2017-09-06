open Basic
open Cfg
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

let () =
  inspect fib_cfg_1;
  (* fib_cfg_1 lacks source information *)
  assert (not (equal fib_cfg_1 fib_cfg_2));
  assert (equal fib_cfg_1 (discard_source_info fib_cfg_2))
