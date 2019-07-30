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

let string_of_binop = function
  | Plus -> "+"
  | Minus -> "-"
  | Mul -> "*"
  | Div -> "/"
  | Mod -> "%"

let string_of_relop = function
  | EQ -> "=="
  | NE -> "!="
  | LT -> "<"
  | GT -> ">"
  | LE -> "<="
  | GE -> ">="

let rec string_of_expr = function
  | Const n -> string_of_int n
  | Ref (Var x) -> x
  | Binop (op, e1, e2) ->
    string_of_expr e1 ^ " " ^ string_of_binop op ^ " " ^ string_of_expr e2
  | Relop (op, e1, e2) ->
    string_of_expr e1 ^ " " ^ string_of_relop op ^ " " ^ string_of_expr e2

let string_of_stmt ?(indent = 0) stmt =
  let indent = String.make indent ' ' in
  match stmt with
  | Move (Var x, e) ->
    indent ^ x ^ " := " ^ string_of_expr e
  | Load (Var x, Mem { base = Addr base; offset }) ->
    indent ^ x ^ " := " ^ base ^ "[" ^ string_of_expr offset ^ "]"
  | Store (Mem { base = Addr base; offset }, e) ->
    indent ^ base ^ "[" ^ string_of_expr offset ^ "] := " ^ string_of_expr e
  | Label l ->
    l ^ ":"
  | Jump l ->
    indent ^ "goto " ^ l
  | Cond (e, l) ->
    indent ^ "if " ^ string_of_expr e ^ " goto " ^ l
  | Receive (Var x) ->
    indent ^ "receive " ^ x
  | Return (Some e) ->
    indent ^ "return " ^ string_of_expr e
  | Return None ->
    indent ^ "return"
