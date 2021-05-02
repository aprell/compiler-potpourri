open Three_address_code__IR
open Control_flow

type fun_decl = {
  name : string;
  type_sig : ty * ty list;
}

and ty = Int32 | Ptr of ty | Void

(* Constructor for function declarations *)
let declare ?(return = Void) name ~params =
  { name; type_sig = (return, params); }

let rec string_of_ty = function
  | Int32 -> "i32"
  | Ptr x -> string_of_ty x ^ "*"
  | Void -> "void"

let global name = "@" ^ name

let local name = "%" ^ name

let get_first_basic_block (graph : Cfg.t) =
  let open Cfg in
  let entry = get_entry_node graph in
  assert (NodeSet.cardinal entry.succ = 1);
  (NodeSet.choose entry.succ).block

let print ~indent =
  print_string (String.make indent ' ');
  fun args -> Printf.printf args

let emit_function_declaration { name; type_sig = (return, params) } =
  let return = string_of_ty return in
  let params = List.map string_of_ty params in
    print ~indent:0 "declare %s %s(%s)"
      return (global name) (String.concat ", " params)

let emit_function_header (block : Basic_block.t) (decl : fun_decl) =
  match Basic_block.entry_label block with
  | _, Some params ->
    let ty, tys = decl.type_sig in
    if List.length params = List.length tys then (
      let params = List.map2 (fun param ty ->
          Printf.sprintf "%s %s"
            (string_of_ty ty) (local (name_of_var param))
        ) params tys
      in
      print ~indent:0 "define %s %s(%s)"
        (string_of_ty ty) (global decl.name) (String.concat ", " params)
    ) else (
      failwith "Function signature mismatch"
    )
  | _, None -> failwith "Missing parameter list"

let string_of_binop = function
  | Plus -> "add i32"
  | Minus -> "sub i32"
  | Mul -> "mul i32"
  | Div -> "sdiv i32"
  | Mod -> "srem i32"

let string_of_relop = function
  | EQ -> "icmp eq i32"
  | NE -> "icmp ne i32"
  | LT -> "icmp slt i32"
  | GT -> "icmp sgt i32"
  | LE -> "icmp sle i32"
  | GE -> "icmp sge i32"

let rec string_of_expr = function
  | Const n -> string_of_int n
  | Val (Var x) -> local x
  | Binop (op, e1, e2) ->
    string_of_binop op ^ " " ^ string_of_expr e1 ^ ", " ^ string_of_expr e2
  | Relop (op, e1, e2) ->
    string_of_relop op ^ " " ^ string_of_expr e1 ^ ", " ^ string_of_expr e2

let gen_temp = ref (Three_address_code__Utils.gen_name "%" 0)

let emit_basic_block (block : Basic_block.t) =
  let indent = 2 in
  let emit stmt =
    match !stmt with
    | Move (Var x, Const n) ->
      (* x := n -> x := n + 0 *)
      let e = Binop (Plus, Const n, Const 0) in
      print ~indent "%s = %s\n"
        (local x) (string_of_expr e)
    | Move (Var x, Val y) ->
      (* x := y -> x := y + 0 *)
      let e = Binop (Plus, Val y, Const 0) in
      print ~indent "%s = %s\n"
        (local x) (string_of_expr e)
    | Move (Var x, e) ->
      print ~indent "%s = %s\n"
        (local x) (string_of_expr e)
    | Load (Var x, Deref (Var y)) ->
      print ~indent "%s = load i32, i32* %s\n"
        (local x) (local y)
    | Store (Deref (Var x), e) ->
      print ~indent "store i32 %s, i32* %s\n"
        (string_of_expr e) (local x)
    | Label (name, _) ->
      print ~indent:0 "%s:\n"
        name
    | Jump (label, _) ->
      print ~indent "br label %s\n"
        (local label)
    | Cond (Binop _ as e, (iftrue, _), (iffalse, _))
    | Cond (Relop _ as e, (iftrue, _), (iffalse, _)) ->
      let tmp = !gen_temp () in
      print ~indent "%s = %s\n"
        tmp (string_of_expr e);
      print ~indent "br i1 %s, label %s, label %s\n"
        tmp (local iftrue) (local iffalse)
    | Cond (e, (iftrue, _), (iffalse, _)) ->
      print ~indent "br i1 %s, label %s, label %s\n"
        (string_of_expr e) (local iftrue) (local iffalse)
    | Return (Some e) ->
      print ~indent "ret i32 %s\n"
        (string_of_expr e)
    | Return None ->
      print ~indent "ret void\n"
    | Phi (x, xs) ->
      let args = List.map2 (fun (Var x) pred ->
          let label, _ = Basic_block.entry_label pred in
          Printf.sprintf "[ %s, %s ]" (local x) (local label)
        ) xs block.pred
      in
      print ~indent "%s = phi i32 %s\n"
        (local (name_of_var x)) (String.concat ", " args)
    | _ -> ()
  in
  List.iter emit block.stmts

let emit_function_body (graph : Cfg.t) =
  Cfg.iter (fun { block; _ } ->
      emit_basic_block block
    ) graph

let emit_function (graph : Cfg.t) (decl : fun_decl) =
  (* Unnamed temporaries are numbered %0, %1, etc., assuming that all function
   * parameters are named and all basic blocks are labeled. *)
  gen_temp := Three_address_code__Utils.gen_name "%" 0;
  emit_function_header (get_first_basic_block graph) decl;
  print_endline " {";
  emit_function_body graph;
  print_endline "}"