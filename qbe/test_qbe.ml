open Three_address_code__Utils

let () =
  List.iter (Qbe.emit >> print_newline) [
    "examples/fib.hir";
    "examples/pow.hir";
    "examples/fastpow.hir";
    "examples/search.hir";
    "examples/sort.hir";
    "examples/test01.hir";
    "examples/test02.hir";
    "examples/test03.hir";
    "examples/test04.hir";
    "examples/test05.hir";
    "examples/test06.hir";
    "examples/test07.hir";
  ]
