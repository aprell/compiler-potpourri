open Three_address_code__Utils

let () =
  List.iter (Llvm.emit >> print_newline) [
    "examples/fib.hir";
    "examples/pow.hir";
    "examples/fastpow.hir";
    "examples/sum.hir";
    "examples/fastsum.hir";
    "examples/search.hir";
    "examples/sort.hir";
    "examples/test01.hir";
    "examples/test02.hir";
    "examples/test03.hir";
    "examples/test04.hir";
    "examples/test05.hir";
    "examples/test06.hir";
    "examples/test07.hir";
    "examples/test08.hir";
    "examples/test09.hir";
    "examples/test10.hir";
  ]
