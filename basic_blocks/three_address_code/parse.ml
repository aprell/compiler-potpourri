open Utils

(*$< IR *)
(*$T parse_line as p
  p "a := 1"              = Move (Var "a", Const 1)
  p "a := b"              = Move (Var "a", Ref (Var "b"))
  p "a := a + 1"          = Move (Var "a", Binop (Plus, Ref (Var "a"), Const 1))
  p "a := b > 1"          = Move (Var "a", Relop (GT, Ref (Var "b"), Const 1))
  p "x := M[0]"           = Load (Var "x", Mem {base = Addr "M"; offset = Const 0})
  p "x := M[y]"           = Load (Var "x", Mem {base = Addr "M"; offset = Ref (Var "y")})
  p "M[0] := 3"           = Store (Mem {base = Addr "M"; offset = Const 0}, Const 3)
  p "M[x] := y"           = Store (Mem {base = Addr "M"; offset = Ref (Var "x")}, Ref (Var "y"))
  p "L0:"                 = Label ("L0", None)
  p "L1():"               = Label ("L1", Some [])
  p "L2(x, y):"           = Label ("L2", Some [Var "x"; Var "y"])
  p "goto L0"             = Jump ("L0", None)
  p "goto L1()"           = Jump ("L1", Some [])
  p "goto L2(a, b)"       = Jump ("L2", Some [Var "a"; Var "b"])
  p "if 0 goto L0"        = Cond (Const 0, ("L0", None))
  p "if a != 0 goto L1"   = Cond (Relop (NE, Ref (Var "a"), Const 0), ("L1", None))
  p "receive n"           = Receive (Var "n")
  p "return"              = Return None
  p "return 0"            = Return (Some (Const 0))
  p "return n"            = Return (Some (Ref (Var "n")))
*)
(*$>*)
let parse_line line =
  let lexbuf = Lexing.from_string line in
  Parser.line Lexer.read lexbuf

let parse_prog lines =
  let lexbuf = Lexing.from_string (String.concat "\n" lines) in
  Parser.prog Lexer.read lexbuf

let parse_file name =
  read_file name |> parse_prog
