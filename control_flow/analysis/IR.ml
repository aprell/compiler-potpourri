type stmt =
  | Move of var * expr        (* x := e      *)
  | Load of var * mem         (* x := M[i]   *)
  | Store of mem * expr       (* M[i] := e   *)
  | Label of name             (* L:          *)
  | Jump of label             (* goto L      *)
  | Cond of expr * label      (* if e goto L *)
  | Receive of var            (* receive x   *)
  | Return of expr option     (* return e    *)

and expr =
  | Const of int
  | Ref of var
  | Binop of binop * expr * expr
  | Relop of relop * expr * expr

and var = Var of name

and mem = Mem of { base : addr;  offset : expr }

and addr = Addr of name

and binop = Plus | Minus | Mul | Div | Mod

and relop = EQ | NE | LT | GT | LE | GE

and label = name

and name = string
