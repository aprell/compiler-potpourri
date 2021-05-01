open Control_flow
open Graphs

let emit_fib () =
  let graph = graph_of_input "examples/fib.hir" in
  let _ = Ssa.construct graph in
  Llvm.declare "fib"
    ~return:Llvm.Int32
    ~params:[Llvm.Int32]
  |> Llvm.emit_function graph

let emit_pow () =
  let graph = graph_of_input "examples/pow.hir" in
  let _ = Ssa.construct graph in
  Llvm.declare "pow"
    ~return:Llvm.Int32
    ~params:[Llvm.Int32; Llvm.Int32]
  |> Llvm.emit_function graph

let emit_fastpow () =
  let graph = graph_of_input "examples/fastpow.hir" in
  let _ = Ssa.construct graph in
  Llvm.declare "fastpow"
    ~return:Llvm.Int32
    ~params:[Llvm.Int32; Llvm.Int32]
  |> Llvm.emit_function graph

let emit_sort () =
  let graph = graph_of_input "examples/sort.hir" in
  let _ = Ssa.construct graph in
  Llvm.declare "sort"
    ~params:[Llvm.(Ptr Int32); Llvm.Int32]
  |> Llvm.emit_function graph

let emit_test01 () =
  let graph = graph_of_input "examples/test01.hir" in
  let _ = Ssa.construct graph in
  Llvm.declare "test01"
    ~return:Llvm.Int32
    ~params:[Llvm.Int32; Llvm.Int32]
  |> Llvm.emit_function graph

let emit_test02 () =
  let graph = graph_of_input "examples/test02.hir" in
  let _ = Ssa.construct graph in
  Llvm.declare "test02"
    ~return:Llvm.Int32
    ~params:[]
  |> Llvm.emit_function graph

let emit_test03 () =
  let graph = graph_of_input "examples/test03.hir" in
  let _ = Ssa.construct graph in
  Llvm.declare "test03"
    ~params:[Llvm.Int32]
  |> Llvm.emit_function graph

let emit_test04 () =
  let graph = graph_of_input "examples/test04.hir" in
  let _ = Ssa.construct graph in
  Llvm.declare "test04"
    ~params:[Llvm.(Ptr Int32); Llvm.Int32]
  |> Llvm.emit_function graph

let emit_test05 () =
  let graph = graph_of_input "examples/test05.hir" in
  let _ = Ssa.construct graph in
  Llvm.declare "test05"
    ~return:Llvm.Int32
    ~params:[]
  |> Llvm.emit_function graph

let emit_test06 () =
  let graph = graph_of_input "examples/test06.hir" in
  let _ = Ssa.construct graph in
  Llvm.declare "test06"
    ~return:Llvm.Int32
    ~params:[Llvm.Int32]
  |> Llvm.emit_function graph

let emit_test07 () =
  let graph = graph_of_input "examples/test07.hir" in
  let _ = Ssa.construct graph in
  Llvm.declare "test07"
    ~return:Llvm.Int32
    ~params:[Llvm.Int32]
  |> Llvm.emit_function graph

let () =
  emit_fib ();
  print_newline ();

  emit_pow ();
  print_newline ();

  emit_fastpow ();
  print_newline ();

(*
  emit_sort ();
  print_newline ();
*)

  emit_test01 ();
  print_newline ();

  emit_test02 ();
  print_newline ();

  emit_test03 ();
  print_newline ();

(*
  emit_test04 ();
  print_newline ();
*)

  emit_test05 ();
  print_newline ();

  emit_test06 ();
  print_newline ();

  emit_test07 ()
