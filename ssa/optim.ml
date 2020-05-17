open Three_address_code__IR

(* TODO: Add missing cases *)
let constant_fold = function
  | Binop (Plus, Const n, Const m) -> Const (n + m)
  | Binop (Minus, Const n, Const m) -> Const (n - m)
  | e -> e

(* TODO: Add missing cases *)
let rec replace_expr x y = function
  | Ref x' when x = x' -> y
  | Binop (op, e1, e2) ->
    constant_fold (Binop (op, replace_expr x y e1, replace_expr x y e2))
  | Relop (op, e1, e2) ->
    constant_fold (Relop (op, replace_expr x y e1, replace_expr x y e2))
  | e -> e

(* TODO: Add missing cases *)
let replace_stmt x y = function
  | Move (x', e) -> Move (x', replace_expr x y e)
  | Return (Some e) -> Return (Some (replace_expr x y e))
  | s -> s

(* Eliminate moves with constant propagation (1) and copy propagation (2):
 * (1) x := c => replace Ref (Var x) with Const c in stmts
 * (2) x := y => replace Ref (Var x) with Ref (Var y) in stmts *)
let propagate move stmts =
  let x, y = match move with
    | Move (x, y) -> x, y
    | _ -> failwith "propagate"
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
