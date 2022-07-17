open Utils

(*$< IR *)
(*$T parse_line as p
  p "a := 1" = \
   Move (Var "a", Const 1)

  p "a := -1" = \
   Move (Var "a", Const (-1))

  p "a := b" = \
   Move (Var "a", Val (Var "b"))

  p "a := -b" = \
   Move (Var "a", Binop (Minus, Const 0, Val (Var "b")))

  p "a := a + 1" = \
   Move (Var "a", Binop (Plus, Val (Var "a"), Const 1))

  p "a := b > 1" = \
   Move (Var "a", Relop (GT, Val (Var "b"), Const 1))

  p "L0:" = \
   Label ("L0", None)

  p "L1():" = \
   Label ("L1", Some [])

  p "L2(x, y):" = \
   Label ("L2", Some [Var "x"; Var "y"])

  p "goto L0" = \
   Jump ("L0", None)

  p "goto L1()" = \
   Jump ("L1", Some [])

  p "goto L2(a, b)" = \
   Jump ("L2", Some [Var "a"; Var "b"])

  p "if 0 goto L0 else goto L1" = \
   Cond (Const 0, ("L0", None), ("L1", None))

  p "if a != 0 goto L0 else goto L1" = \
   Cond (Relop (NE, Val (Var "a"), Const 0), ("L0", None), ("L1", None))

  p "return" = \
   Return None

  p "return 0" = \
   Return (Some (Const 0))

  p "return n" = \
   Return (Some (Val (Var "n")))
*)
(*$>*)
let parse_line line =
  let lexbuf = Lexing.from_string line in
  Parser.line Lexer.read lexbuf

let parse_prog lines =
  let lexbuf = Lexing.from_string lines in
  let _, stmts = Parser.prog Lexer.read lexbuf in
  stmts

let parse_file name =
  read_file_into_string name |> parse_prog
