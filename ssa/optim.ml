open Three_address_code__IR

(* TODO: Add missing cases *)
let constant_fold = function
  | Binop (Plus, Const n, Const m) -> Const (n + m)
  | Binop (Minus, Const n, Const m) -> Const (n - m)
  | Binop (Mul, Const n, Const m) -> Const (n * m)
  | Binop (Div, Const n, Const m) ->
    if m <> 0 then Const (n / m) else failwith "Division by zero"
  | e -> e

(* TODO: Add missing cases *)
let rec replace_expr x y = function
  | Ref x' when x' = x -> Ref y
  | Binop (op, e1, e2) ->
    constant_fold (Binop (op, replace_expr x y e1, replace_expr x y e2))
  | Relop (op, e1, e2) ->
    constant_fold (Relop (op, replace_expr x y e1, replace_expr x y e2))
  | e -> e

let replace_list x y =
  List.map (fun z -> if z = x then y else z)

(* TODO: Add missing cases *)
let replace_stmt x y = function
  | Move (x', e) ->
    Move (x', replace_expr x y e)
  | Jump (l, Some xs) ->
    Jump (l, Some (replace_list x y xs))
  | Cond (e, (l1, Some xs), (l2, Some ys)) ->
    Cond (replace_expr x y e, (l1, Some (replace_list x y xs)), (l2, Some (replace_list x y ys)))
  | Return (Some e) ->
    Return (Some (replace_expr x y e))
  | Phi (x', xs) ->
    Phi (x', replace_list x y xs)
  | s -> s

(* Eliminate moves with constant propagation (1) and copy propagation (2):
 * (1) x := c => replace Ref (Var x) with Const c in stmts
 * (2) x := y => replace Ref (Var x) with Ref (Var y) in stmts *)
let propagate move stmts =
  let x, y = match move with
    | Move (x, Ref y) -> x, y
    | _ -> invalid_arg "propagate"
  in
  let rec loop acc = function
    | stmt :: stmts -> loop (replace_stmt x y stmt :: acc) stmts
    | [] -> List.rev acc
  in
  loop [] stmts

let rec optimize = function
  | Move (_, Const _) as const :: stmts ->
    optimize (propagate const stmts)
  | Move (_, Ref (Var _)) as copy :: stmts ->
    optimize (propagate copy stmts)
  | stmt :: stmts ->
    stmt :: optimize stmts
  | [] -> []
