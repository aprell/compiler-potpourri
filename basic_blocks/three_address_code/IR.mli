type stmt =
  | Move of var * expr               (* x := e                     *)
  | Load of var * mem                (* x := M[i]                  *)
  | Store of mem * expr              (* M[i] := e                  *)
  | Label of label                   (* L:                         *)
  | Jump of label                    (* goto L                     *)
  | Cond of expr * label * label     (* if e goto L1 else goto L2  *)
  | Return of expr option            (* return e                   *)
  (* Phi-functions (SSA) *)
  | Phi of var * var list            (* x := PHI(...)              *)

and expr =
  | Const of int
  | Val of var
  | Binop of binop * expr * expr
  | Relop of relop * expr * expr

and var = Var of name

and mem = Mem of base * offset

and base = var

and offset = expr

and binop = Plus | Minus | Mul | Div | Mod

and relop = EQ | NE | LT | GT | LE | GE

and label = name * var list option

and name = string

module Type : sig
  type t =
    | Int
    | Void
    | Ptr of t
end

type decl = FunDecl of { name : string; typesig : Type.t * Type.t list; }

val name_of_var : var -> name

val make_label : ?name:name -> ?params:var list -> unit -> label

val is_phi : stmt -> bool

val normalize : stmt -> stmt list

val lower :
  [> `Function of name * var list * stmt list
  | `Load of var * [< `Addr of var * expr ]
  | `Store of [< `Addr of var * expr ] * expr
  | `If of expr * stmt list * stmt list
  | `While of expr * stmt list ]
  -> stmt list

val constant_fold : expr -> expr

module Vars : Set.S with type elt = var

val collect_variables : expr -> Vars.t

val replace : var -> expr -> stmt:stmt ref ref -> unit

val string_of_expr : expr -> string

val string_of_stmt : ?indent:int -> stmt -> string

val dump : stmt list -> unit
