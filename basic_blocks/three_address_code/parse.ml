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

  p "x := PHI()" = \
   Phi (Var "x", [])

  p "x := PHI(y)" = \
   Phi (Var "x", [Var "y"])

  p "x := PHI(y, z)" = \
   Phi (Var "x", [Var "y"; Var "z"])
*)
(*$>*)
let parse_line line =
  let lexbuf = Lexing.from_string line in
  let _, stmts = Parser.prog Lexer.read lexbuf in
  List.hd stmts

let print_error input (Lexing.{ lex_curr_pos = pos; _ } as lexbuf) =
  let sol = (* start of line *)
    try String.rindex_from input (pos - 1) '\n' + 1 with
      Not_found -> 0
  in
  let eol = (* end of line *)
    try String.index_from input pos '\n' with
      Not_found -> String.length input
  in
  let len = (* length of matched string *)
    String.length (Lexing.lexeme lexbuf)
  in
  Printf.eprintf "%s\n%s\n%!"
    (String.sub input sol (eol - sol))
    (String.make (pos - sol - len) ' ' ^ String.make len '^')

let parse_prog input =
  let lexbuf = Lexing.from_string input in
  try Parser.prog Lexer.read lexbuf with
  | Lexer.Error msg as e ->
    let line, col = Lexer.position lexbuf in
    Printf.eprintf "Syntax error in line %d, column %d: %s:\n%!" line col msg;
    print_error input lexbuf;
    raise e
  | Parser.Error as e ->
    let line, col = Lexer.position lexbuf in
    Printf.eprintf "Parse error in line %d, column %d:\n%!" line col;
    print_error input lexbuf;
    raise e

let parse_file name =
  read_file_into_string name |> parse_prog
