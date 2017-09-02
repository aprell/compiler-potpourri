type stmt =
  | Move of name * expr
  | Fetch of name * mem
  | Store of mem * scalar
  | Label of name
  | Jump of label
  | Cond of expr * label
  | Receive of scalar option
  | Return of scalar option
  | Input of scalar
  | Output of scalar

and expr =
  | Val of scalar
  | Binop of binop * scalar * scalar
  | Relop of relop * scalar * scalar

and scalar =
  | Const of int
  | Var of name

and mem = Mem of { base : name;  index : scalar }

and label = Target of name

and binop = Plus | Minus | Mul | Div | Mod

and relop = EQ | NE | LT | GT | LE | GE

and name = string
