type stmt =
  | Move of var * expr                          (* x := e                     *)
  | Load of var * mem                           (* x := M[i]                  *)
  | Store of mem * expr                         (* M[i] := e                  *)
  | Label of label                              (* L:                         *)
  | Jump of label                               (* goto L                     *)
  | Cond of expr * label * label                (* if e goto L1 else goto L2  *)
  | Receive of var                              (* receive x                  *)
  | Return of expr option                       (* return e                   *)
  (* Phi-functions (SSA) *)
  | Phi of var * var list                       (* x := PHI(...)              *)

and expr =
  | Const of int
  | Val of var
  | Binop of binop * expr * expr
  | Relop of relop * expr * expr

and var = Var of name

and mem = Deref of var

and binop = Plus | Minus | Mul | Div | Mod

and relop = EQ | NE | LT | GT | LE | GE

and label = name * var list option

and name = string

val name_of_var : var -> name

val is_phi : stmt -> bool

val lower :
  [> `If of expr * stmt list * stmt list
  | `Load of var * [< `Addr of var * expr ]
  | `Proc of name * var list * stmt list
  | `Store of [< `Addr of var * expr ] * expr
  | `While of expr * stmt list ]
  -> stmt list

val constant_fold : expr -> expr

val all_variables_expr : expr -> var list

val all_variables_stmt : stmt -> var list

val all_variables : stmt list -> var list

val string_of_expr : expr -> string

val string_of_stmt : ?indent:int -> stmt -> string

val dump : stmt list -> unit
