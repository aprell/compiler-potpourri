open Utils

(*$< IR *)
(*$T parse_line as p
  p "a := 1"              = Move ("a", Val (Const 1))
  p "a := b"              = Move ("a", Val (Var "b"))
  p "a := a + 1"          = Move ("a", Binop (Plus, Var "a", Const 1))
  p "a := b > 1"          = Move ("a", Relop (GT, Var "b", Const 1))
  p "x := M[0]"           = Fetch ("x", Mem {base = "M"; index = Const 0})
  p "x := M[y]"           = Fetch ("x", Mem {base = "M"; index = Var "y"})
  p "M[0] := 3"           = Store (Mem {base = "M"; index = Const 0}, Const 3)
  p "M[x] := y"           = Store (Mem {base = "M"; index = Var "x"}, Var "y")
  p "L0:"                 = Label "L0"
  p "exit:"               = Label "exit"
  p "goto L0"             = Jump (Target "L0")
  p "goto exit"           = Jump (Target "exit")
  p "if 0 goto L0"        = Cond (Val (Const 0), Target "L0")
  p "if a != 0 goto exit" = Cond (Relop (NE, Var "a", Const 0), Target "exit")
  p "receive"             = Receive None
  p "receive n"           = Receive (Some (Var "n"))
  p "return"              = Return None
  p "return 0"            = Return (Some (Const 0))
  p "return n"            = Return (Some (Var "n"))
  p "input n"             = Input (Var "n")
  p "output n"            = Output (Var "n")
*)
(*$>*)
let parse_line line =
  let lexbuf = Lexing.from_string line in
  Parser.prog Lexer.read lexbuf

let parse_file name =
  read_file name |> List.map parse_line
