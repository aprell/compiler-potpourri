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

let translate (`Addr (base, index)) =
  (* base + index * 4 *)
  match index with
  | Const i when i = 0 -> [], Deref base
  | Const i ->
    let t1 = gen_temp () in
    [ Move (Var t1, (Binop (Plus, Val base, Const (i * 4)))) ]
    , Deref (Var t1)
  | Val _ ->
    let t1 = gen_temp () in
    let t2 = gen_temp () in
    [ Move (Var t1, Val base);
      Move (Var t2, Binop (Mul, index, Const 4));
      Move (Var t2, Binop (Plus, Val (Var t1), Val (Var t2))); ]
    , Deref (Var t2)
  | _ ->
    let t1 = gen_temp () in
    let t2 = gen_temp () in
    [ Move (Var t1, Val base);
      Move (Var t2, index);
      Move (Var t2, Binop (Mul, Val (Var t2), Const 4));
      Move (Var t2, Binop (Plus, Val (Var t1), Val (Var t2))); ]
    , Deref (Var t2)

let lower = function
  | `Proc (name, params, body) ->
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
    @ [Move (Var t1, e)]
    @ [Store (addr', Val (Var t1))]
  | `If (Relop _ as e, then_, []) ->
    let l1 = make_label () in
    let l2 = make_label () in
    [Cond (e, l1, l2)]
    @ [Label l1]
    @ then_
    @ [Label l2]
  | `If (Relop _ as e, then_, else_) ->
    let l1 = make_label () in
    let l2 = make_label () in
    let l3 = make_label () in
    [Cond (e, l1, l2)]
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
    @ [Cond (e, l2, l3)]
    @ [Label l2]
    @ body
    @ [Jump l1]
    @ [Label l3]
  | _ -> failwith "lower: unsupported statement"

(* TODO: Add missing cases *)
let constant_fold = function
  | Binop (Plus, Const n, Const m) -> Const (n + m)
  | Binop (Minus, Const n, Const m) -> Const (n - m)
  | Binop (Mul, Const n, Const m) -> Const (n * m)
  | Binop (Div, Const n, Const m) ->
    if m <> 0 then Const (n / m) else failwith "Division by zero"
  | Binop (Plus, Val x, Const 0)
  | Binop (Plus, Const 0, Val x)
  | Binop (Minus, Val x, Const 0)
  | Binop (Mul, Val x, Const 1)
  | Binop (Mul, Const 1, Val x)
  | Binop (Div, Val x, Const 1) -> Val x
  | Binop (Div, Val x, Val y) when x = y -> Const 1
  | Binop (Mul, Val _, Const 0)
  | Binop (Mul, Const 0, Val _) -> Const 0
  | Relop (EQ, Const n, Const m) when n = m -> Const 1
  | Relop (EQ, Const n, Const m) when n <> m -> Const 0
  | Relop (NE, Const n, Const m) when n = m -> Const 0
  | Relop (NE, Const n, Const m) when n <> m -> Const 1
  | Relop (LT, Const n, Const m) when n < m -> Const 1
  | Relop (LT, Const n, Const m) when n >= m -> Const 0
  | Relop (GT, Const n, Const m) when n > m -> Const 1
  | Relop (GT, Const n, Const m) when n <= m -> Const 0
  | Relop (LE, Const n, Const m) when n <= m -> Const 1
  | Relop (LE, Const n, Const m) when n > m -> Const 0
  | Relop (GE, Const n, Const m) when n >= m -> Const 1
  | Relop (GE, Const n, Const m) when n < m -> Const 0
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

let rec replace_expr x y = function
  | Val x' when x' = x -> y
  | Binop (op, e1, e2) ->
    constant_fold (Binop (op, replace_expr x y e1, replace_expr x y e2))
  | Relop (op, e1, e2) ->
    constant_fold (Relop (op, replace_expr x y e1, replace_expr x y e2))
  | e -> e

let replace_list x = function
  | Val y -> List.map (fun z -> if z = x then y else z)
  | _ -> invalid_arg "replace_list"

let replace_stmt x y = function
  | Move (x', e) ->
    Move (x', replace_expr x y e)
  | Load (x', Deref x'') as load -> (
      match y with
      | Val y when x = x'' -> Load (x', Deref y)
      | _ -> load
    )
  | Store (Deref x', e) -> (
      let e' = replace_expr x y e in
      match y with
      | Val y when x = x' -> Store (Deref y, e')
      | _ -> Store (Deref x', e')
    )
  | Jump (l, Some xs) ->
    Jump (l, Some (replace_list x y xs))
  | Cond (e, (l1, None), (l2, None)) ->
    Cond (replace_expr x y e, (l1, None), (l2, None))
  | Cond (e, (l1, Some xs), (l2, Some ys)) ->
    Cond (replace_expr x y e, (l1, Some (replace_list x y xs)), (l2, Some (replace_list x y ys)))
  | Return (Some e) ->
    Return (Some (replace_expr x y e))
  | Phi (x', xs) ->
    Phi (x', replace_list x y xs)
  | s -> s

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
  let indent = String.make indent ' ' in
  match stmt with
  | Move (Var x, e) ->
    indent ^ x ^ " := " ^ string_of_expr e
  | Load (Var x, Deref (Var y)) ->
    indent ^ x ^ " := " ^ "*" ^ y
  | Store (Deref (Var x), e) ->
    indent ^ "*" ^ x ^ " := " ^ string_of_expr e
  | Label l ->
    string_of_label l ^ ":"
  | Jump l ->
    indent ^ "goto " ^ string_of_label l
  | Cond (e, l1, l2) ->
    indent ^ "if " ^ string_of_expr e
    ^ " goto " ^ string_of_label l1
    ^ " else goto " ^ string_of_label l2
  | Receive (Var x) ->
    indent ^ "receive " ^ x
  | Return (Some e) ->
    indent ^ "return " ^ string_of_expr e
  | Return None ->
    indent ^ "return"
  | Phi (Var x, xs) ->
    let xs = List.map name_of_var xs in
    indent ^ x ^ " := PHI(" ^ String.concat ", " xs ^ ")"

let dump stmts =
  List.map (string_of_stmt ~indent:4) stmts
  |> String.concat "\n"
  |> print_endline
