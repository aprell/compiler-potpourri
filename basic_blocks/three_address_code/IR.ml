open Utils

type stmt =
  | Move of var * expr                          (* x := e                     *)
  | Load of var * mem                           (* x := M[i]                  *)
  | Store of mem * expr                         (* M[i] := e                  *)
  | Label of label                              (* L:                         *)
  | Jump of label                               (* goto L                     *)
  | Cond of expr * label * label                (* if e goto L1 else goto L2  *)
  | Return of expr option                       (* return e                   *)
  (* Phi-functions (SSA) *)
  | Phi of var * var list                       (* x := PHI(...)              *)

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

module Type = struct
  type t =
    | Int
    | Void
    | Ptr of t
end

type decl = FunDecl of { name : string; typesig : Type.t * Type.t list; }

let name_of_var (Var x) = x

let is_phi = function
  | Phi _ -> true
  | _ -> false

let gen_label = Utils.gen_name "L" 1

(* Constructor for labels *)
let make_label ?(name = gen_label ()) ?params () : label = (name, params)

(* The parser doesn't accept identifiers that start with '$', so these
 * temporaries are guaranteed to not conflict with other identifiers. *)
let gen_temp = Utils.gen_name "$" 1

(* Rewrite nontrivial expressions *)
let rec normalize = function
  | Move (x, Binop (op, (Binop _ as e1), e2)) ->
    let tmp = gen_temp () in
    normalize (Move (Var tmp, e1))
    @ normalize @@ Move (x, Binop (op, Val (Var tmp), e2))
  | Move (x, Binop (op, e1, (Binop _ as e2))) ->
    let tmp = gen_temp () in
    normalize (Move (Var tmp, e2))
    @ normalize @@ Move (x, Binop (op, e1, Val (Var tmp)))
  | Cond (Relop (op, (Binop _ as e1), e2), l1, l2) ->
    let tmp = gen_temp () in
    normalize (Move (Var tmp, e1))
    @ normalize @@ (Cond (Relop (op, Val (Var tmp), e2), l1, l2))
  | Cond (Relop (op, e1, (Binop _ as e2)), l1, l2) ->
    let tmp = gen_temp () in
    normalize (Move (Var tmp, e2))
    @ normalize @@ (Cond (Relop (op, e1, Val (Var tmp)), l1, l2))
  | Return (Some (Binop _ as e)) ->
    let tmp = gen_temp () in
    normalize (Move (Var tmp, e))
    @ [Return (Some (Val (Var tmp)))]
  | s -> [s]

let translate (`Addr (base, index)) =
  (* base + index * 4 *)
  match index with
  | Const i when i = 0 ->
    [], Mem (base, Const 0)
  | Const i ->
    [], Mem (base, Const (i * 4))
  | Val _ ->
    let t1 = gen_temp () in
    [Move (Var t1, Binop (Mul, index, Const 4))]
    , Mem (base, Val (Var t1))
  | _ ->
    let t1 = gen_temp () in
    let t2 = gen_temp () in
    normalize (Move (Var t1, index))
    @ [Move (Var t2, Binop (Mul, Val (Var t1), Const 4))]
    , Mem (base, Val (Var t2))

let lower = function
  | `Function (name, params, body) ->
    [Label (make_label ~name ~params ())]
    @ body
  | `Load (x, addr) ->
    let stmts, addr' = translate addr in
    stmts
    @ [Load (x, addr')]
  | `Store (addr, e) ->
    let stmts, addr' = translate addr in
    let t1 = gen_temp () in
    stmts
    @ normalize (Move (Var t1, e))
    @ [Store (addr', Val (Var t1))]
  | `If (Relop _ as e, then_, []) ->
    let l1 = make_label () in
    let l2 = make_label () in
    normalize (Cond (e, l1, l2))
    @ [Label l1]
    @ then_
    @ [Label l2]
  | `If (Relop _ as e, then_, else_) ->
    let l1 = make_label () in
    let l2 = make_label () in
    let l3 = make_label () in
    normalize (Cond (e, l1, l2))
    @ [Label l1]
    @ then_
    @ [Jump l3]
    @ [Label l2]
    @ else_
    @ [Label l3]
  | `If (Const 0, _, else_) -> else_
  | `If (Const _, then_, _) -> then_
  | `While (Relop _ as e, body) ->
    let l1 = make_label () in
    let l2 = make_label () in
    let l3 = make_label () in
    [Label l1]
    @ normalize (Cond (e, l2, l3))
    @ [Label l2]
    @ body
    @ [Jump l1]
    @ [Label l3]
  | _ -> failwith "lower: unsupported statement"

let constant_fold = function
  | (* x + 0 = x *) Binop (Plus, Val x, Const 0)
  | (* 0 + x = x *) Binop (Plus, Const 0, Val x)
  | (* x - 0 = x *) Binop (Minus, Val x, Const 0)
  | (* x * 1 = x *) Binop (Mul, Val x, Const 1)
  | (* 1 * x = x *) Binop (Mul, Const 1, Val x)
  | (* x / 1 = x *) Binop (Div, Val x, Const 1) -> Val x
  | (* x * 0 = 0 *) Binop (Mul, Val _, Const 0)
  | (* 0 * x = 0 *) Binop (Mul, Const 0, Val _) -> Const 0
  | (* x / x = 1 *) Binop (Div, Val x, Val y) when x = y -> Const 1

  | Binop (Plus, Const n, Const m) -> Const (n + m)
  | Binop (Minus, Const n, Const m) -> Const (n - m)
  | Binop (Mul, Const n, Const m) -> Const (n * m)
  | Binop (Div, Const n, Const m) when m <> 0 -> Const (n / m)
  | Binop (Div, _, Const 0) -> failwith "Division by zero"

  | Relop (EQ, Const n, Const m) -> Const (if n = m then 1 else 0)
  | Relop (NE, Const n, Const m) -> Const (if n = m then 0 else 1)
  | Relop (LT, Const n, Const m) -> Const (if n < m then 1 else 0)
  | Relop (GT, Const n, Const m) -> Const (if n > m then 1 else 0)
  | Relop (LE, Const n, Const m) -> Const (if n > m then 0 else 1)
  | Relop (GE, Const n, Const m) -> Const (if n < m then 0 else 1)
  | e -> e

module Vars = Set.Make (struct
  type t = var
  let compare = Stdlib.compare
end)

let rec collect_variables = function
  | Const _ -> Vars.empty
  | Val x -> Vars.singleton x
  | Binop (_, e1, e2)
  | Relop (_, e1, e2) ->
    Vars.union (collect_variables e1) (collect_variables e2)

let rec replace_expr f = function
  | Val x -> f x
  | Binop (op, e1, e2) ->
    constant_fold (Binop (op, replace_expr f e1, replace_expr f e2))
  | Relop (op, e1, e2) ->
    constant_fold (Relop (op, replace_expr f e1, replace_expr f e2))
  | e -> e

let replace_list f =
  List.map (f >> function | Val x -> x | _ -> assert false)

let replace_stmt f = function
  | Move (x, e) ->
    Move (x, replace_expr f e)
  | Load (x, Mem (b, o)) -> (
      match f b with
      | Val b -> Load (x, Mem (b, replace_expr f o))
      | _ -> assert false
    )
  | Store (Mem (b, o), e) -> (
      match f b with
      | Val b -> Store (Mem (b, replace_expr f o), replace_expr f e)
      | _ -> assert false
    )
  | Jump (l, Some xs) ->
    Jump (l, Some (replace_list f xs))
  | Cond (e, (l1, None), (l2, None)) ->
    Cond (replace_expr f e, (l1, None), (l2, None))
  | Cond (e, (l1, Some xs), (l2, Some ys)) ->
    Cond (replace_expr f e, (l1, Some (replace_list f xs)), (l2, Some (replace_list f ys)))
  | Return (Some e) ->
    Return (Some (replace_expr f e))
  | Phi (x, xs) ->
    Phi (x, replace_list f xs)
  | s -> s

let replace (x : var) (e : expr) =
  let f y = if x = y then e else Val y in
  fun ~stmt -> !stmt := replace_stmt f !(!stmt)

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

let string_of_label = function
  | name, None -> name
  | name, Some params ->
    let params = List.map name_of_var params in
    name ^ "(" ^ String.concat ", " params ^ ")"

let rec string_of_expr = function
  | Const n -> string_of_int n
  | Val (Var x) -> x
  | Binop (op, e1, e2) ->
    string_of_expr e1 ^ " " ^ string_of_binop op ^ " " ^ string_of_expr e2
  | Relop (op, e1, e2) ->
    string_of_expr e1 ^ " " ^ string_of_relop op ^ " " ^ string_of_expr e2

let string_of_stmt ?(indent = 0) stmt =
  match stmt with
  | Move (Var x, e) ->
    sprintf ~indent "%s := %s"
      x (string_of_expr e)
  | Load (Var x, Mem (Var b, Const 0)) ->
    sprintf ~indent "%s := *%s"
      x b
  | Load (Var x, Mem (Var b, o)) ->
    sprintf ~indent "%s := *(%s + %s)"
      x b (string_of_expr o)
  | Store (Mem (Var b, Const 0), e) ->
    sprintf ~indent "*%s := %s"
      b (string_of_expr e)
  | Store (Mem (Var b, o), e) ->
    sprintf ~indent "*(%s + %s) := %s"
      b (string_of_expr o) (string_of_expr e)
  | Label l ->
    sprintf "%s:"
      (string_of_label l)
  | Jump l ->
    sprintf ~indent "goto %s"
      (string_of_label l)
  | Cond (e, l1, l2) ->
    sprintf ~indent "if %s goto %s else goto %s"
      (string_of_expr e) (string_of_label l1) (string_of_label l2)
  | Return (Some e) ->
    sprintf ~indent "return %s"
      (string_of_expr e)
  | Return None ->
    sprintf ~indent "return"
  | Phi (Var x, xs) ->
    let xs = List.map name_of_var xs in
    sprintf ~indent "%s := PHI(%s)"
      x (String.concat ", " xs)

let dump stmts =
  List.map (string_of_stmt ~indent:4) stmts
  |> String.concat "\n"
  |> print_endline
