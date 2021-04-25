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
  print sort
