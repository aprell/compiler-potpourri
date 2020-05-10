type proc = Proc of {
    name : string;
    params : var list;
    body : stmt list;
}

and stmt =
  | Move of var * expr
  | Load of var * mem
  | Store of mem * expr
  | Label of label
  | Jump of label
  | Cond of expr * label
  | Receive of var
  | Return of expr option
  (* High-level constructs *)
  | If of expr * stmt list * stmt list option
  | Loop of expr * stmt list

and expr =
  | Const of int
  | Ref of var
  | Binop of binop * expr * expr
  | Relop of relop * expr * expr

and var = Var of name

and mem = Mem of { base : addr; offset : expr; }

and addr = Addr of name

and binop = Plus | Minus | Mul | Div | Mod

and relop = EQ | NE | LT | GT | LE | GE

and label = name * var list option

and name = string

val lower : proc -> stmt list

val all_variables_expr : expr -> var list

val all_variables_stmt : stmt -> var list

val all_variables : stmt list -> var list

val string_of_stmt : ?indent:int -> stmt -> string

val dump : stmt list -> unit
