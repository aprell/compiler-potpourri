type proc = Proc of {
    name : string;
    params : var list;
    body : stmt list;
}

and stmt =
  | Move of var * expr                          (* x := e              *)
  | Load of var * mem                           (* x := M[i]           *)
  | Store of mem * expr                         (* M[i] := e           *)
  | Label of label                              (* L[(...)]:           *)
  | Jump of label                               (* goto L[(...)]       *)
  | Cond of expr * label                        (* if e goto L[(...)]  *)
  | Receive of var                              (* receive x           *)
  | Return of expr option                       (* return e            *)
  (* High-level constructs *)
  | If of expr * stmt list * stmt list option   (* if e ... [else ...] *)
  | Loop of expr * stmt list                    (* while e ...         *)
  (* Phi-functions (SSA) *)
  | Phi of var * var list                       (* x := PHI(...) *)

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

(* Constructor for labels *)
let gen_label ?params name : label = (name, params)

let gen_name = Utils.gen_sym "L" 1

let flip = function
  | EQ -> NE
  | NE -> EQ
  | LT -> GE
  | GT -> LE
  | LE -> GT
  | GE -> LT

let rec lower_stmt = function
  | If (Relop (op, e1, e2), then_, None) ->
    let l1 = gen_label (gen_name ()) in
    [Cond (Relop (flip op, e1, e2), l1)]
    @ (List.flatten @@ List.map lower_stmt then_)
    @ [Label l1]
  | If (Relop (op, e1, e2), then_, Some else_) ->
    let l1 = gen_label (gen_name ()) in
    let l2 = gen_label (gen_name ()) in
    [Cond (Relop (flip op, e1, e2), l1)]
    @ (List.flatten @@ List.map lower_stmt then_)
    @ [Jump l2]
    @ [Label l1]
    @ (List.flatten @@ List.map lower_stmt else_)
    @ [Label l2]
  | If _ -> failwith "lower_stmt: unsupported"
  | Loop (Relop (op, e1, e2), body) ->
    let l1 = gen_label (gen_name ()) in
    let l2 = gen_label (gen_name ()) in
    [Label l1]
    @ [Cond (Relop (flip op, e1, e2), l2)]
    @ (List.flatten @@ List.map lower_stmt body)
    @ [Jump l1]
    @ [Label l2]
  | Loop _ -> failwith "lower_stmt: unsupported"
  | s -> [s]

let lower (Proc { name; params; body }) =
  [Label (gen_label name ~params)]
  @ (List.flatten @@ List.map lower_stmt body)

let rec all_variables_expr = function
  | Const _ -> []
  | Ref x -> [x]
  | Binop (_, e1, e2)
  | Relop (_, e1, e2) ->
    all_variables_expr e1 @ all_variables_expr e2

let all_variables_stmt = function
  | Move (x, e) ->
    x :: all_variables_expr e
  | Load (x, Mem { offset; _ }) ->
    x :: all_variables_expr offset
  | Store (Mem { offset; _ }, e) ->
    all_variables_expr offset @ all_variables_expr e
  | Label _ | Jump _ -> []
  | Cond (e, _) ->
    all_variables_expr e
  | Receive x -> [x]
  | Return (Some e) ->
    all_variables_expr e
  | Return None -> []
  | _ -> assert false

module S = Set.Make (struct
  type t = var
  let compare = Stdlib.compare
end)

let all_variables stmts =
  stmts
  |> List.map all_variables_stmt
  |> List.flatten
  |> S.of_list
  |> S.elements

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
    let params = List.map (fun (Var x) -> x) params in
    name ^ "(" ^ String.concat ", " params ^ ")"

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
    string_of_label l ^ ":"
  | Jump l ->
    indent ^ "goto " ^ string_of_label l
  | Cond (e, l) ->
    indent ^ "if " ^ string_of_expr e ^ " goto " ^ string_of_label l
  | Receive (Var x) ->
    indent ^ "receive " ^ x
  | Return (Some e) ->
    indent ^ "return " ^ string_of_expr e
  | Return None ->
    indent ^ "return"
  | Phi (Var x, xs) ->
    let xs = List.map (fun (Var x) -> x) xs in
    indent ^ x ^ " := PHI(" ^ String.concat ", " xs ^ ")"
  | _ -> assert false

let dump stmts =
  List.map (string_of_stmt ~indent:4) stmts
  |> String.concat "\n"
  |> print_endline
