open Control_flow
open Graphs

let print decl =
  Llvm.string_of_fun_decl decl
  |> print_endline

let () =
  let graph = graph_of_input "examples/fib.hir" in
  let ssa_graph = Ssa.construct graph in

  Llvm.declare "fib"
    ~return:Llvm.Int32
    ~params:[Llvm.Int32]
  |> Llvm.emit_function graph ssa_graph;

  print_newline ();

  let graph = Optim.optimize graph ssa_graph in

  Llvm.declare "fib_optim"
    ~return:Llvm.Int32
    ~params:[Llvm.Int32]
  |> Llvm.emit_function graph ssa_graph
