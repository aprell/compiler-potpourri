open Control_flow
open Graphs

let print decl =
  Llvm.string_of_fun_decl decl
  |> print_endline

let () =
  let fib =
    Llvm.declare "fib"
    ~return:Llvm.Int32
    ~params:[Llvm.Int32]
  in
  let sort =
    Llvm.declare "sort"
    ~params:[Llvm.Ptr Int32; Llvm.Int32]
  in
  print fib;
  print sort;

  let graph = graph_of_input "examples/fib.hir" in
  Llvm.print graph fib;

  let graph = graph_of_input "examples/sort.hir" in
  Llvm.print graph sort
