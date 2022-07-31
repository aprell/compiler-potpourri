open Three_address_code
open Three_address_code__IR
open Three_address_code__Utils
open Control_flow

let rec string_of_type = function
  | Type.Int -> "i32"
  | Type.Void -> "void"
  | Type.Ptr Type.Int -> "i8*"
  | Type.Ptr t -> string_of_type t ^ "*"

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

let global name = "@" ^ name

let local name = "%" ^ name

let rec string_of_expr = function
  | Const n -> string_of_int n
  | Val (Var x) -> local x
  | Binop (op, e1, e2) ->
    string_of_binop op ^ " " ^ string_of_expr e1 ^ ", " ^ string_of_expr e2
  | Relop (op, e1, e2) ->
    string_of_relop op ^ " " ^ string_of_expr e1 ^ ", " ^ string_of_expr e2

let gen_temp = ref (Three_address_code.Utils.gen_name "%" 0)

let emit_basic_block (FunDecl { typesig = (return_type, _); _ }) (block : Basic_block.t) =
  let indent = 2 in
  let emit stmt =
    match !stmt with
    | Move (Var x, Const n) ->
      (* x := n -> x := n + 0 *)
      let e = Binop (Plus, Const n, Const 0) in
      printf ~indent "%s = %s\n"
        (local x) (string_of_expr e)
    | Move (Var x, Val y) ->
      (* x := y -> x := y + 0 *)
      let e = Binop (Plus, Val y, Const 0) in
      printf ~indent "%s = %s\n"
        (local x) (string_of_expr e)
    | Move (Var x, e) ->
      printf ~indent "%s = %s\n"
        (local x) (string_of_expr e)
    | Load (Var x, Mem (Var b, o)) ->
      let tmp1 = !gen_temp () in
      let tmp2 = !gen_temp () in
      printf ~indent "%s = getelementptr i8, i8* %s, i32 %s\n"
        tmp1 (local b) (string_of_expr o);
      printf ~indent "%s = bitcast i8* %s to i32*\n"
        tmp2 tmp1;
      printf ~indent "%s = load i32, i32* %s\n"
        (local x) tmp2
    | Store (Mem (Var b, o), e) ->
      let tmp1 = !gen_temp () in
      let tmp2 = !gen_temp () in
      printf ~indent "%s = getelementptr i8, i8* %s, i32 %s\n"
        tmp1 (local b) (string_of_expr o);
      printf ~indent "%s = bitcast i8* %s to i32*\n"
        tmp2 tmp1;
      printf ~indent "store i32 %s, i32* %s\n"
        (string_of_expr e) tmp2
    | Label (name, _) ->
      printf "%s:\n"
        name
    | Jump (label, _) ->
      printf ~indent "br label %s\n"
        (local label)
    | Cond (Binop _ as e, (iftrue, _), (iffalse, _))
    | Cond (Relop _ as e, (iftrue, _), (iffalse, _)) ->
      let tmp = !gen_temp () in
      printf ~indent "%s = %s\n"
        tmp (string_of_expr e);
      printf ~indent "br i1 %s, label %s, label %s\n"
        tmp (local iftrue) (local iffalse)
    | Cond (e, (iftrue, _), (iffalse, _)) ->
      printf ~indent "br i1 %s, label %s, label %s\n"
        (string_of_expr e) (local iftrue) (local iffalse)
    | Return (Some e) ->
      printf ~indent "ret %s %s\n"
        (string_of_type return_type)
        (string_of_expr e)
    | Return None when return_type <> Type.Void ->
      printf ~indent "ret %s undef\n"
        (string_of_type return_type)
    | Return None ->
      printf ~indent "ret void\n"
    | Phi (x, xs) ->
      let args = List.map2 (fun (Var x) pred ->
          let label, _ = Basic_block.entry_label pred in
          Printf.sprintf "[ %s, %s ]" (local x) (local label)
        ) xs block.pred
      in
      printf ~indent "%s = phi i32 %s\n"
        (local (name_of_var x)) (String.concat ", " args)
  in
  List.iter emit block.stmts

let emit_function_body (decl : IR.decl) (graph : Cfg.t) =
  Cfg.iter (fun { block; _ } ->
      emit_basic_block decl block
    ) graph

let emit_function_header (decl : IR.decl) = function
  | name, Some params ->
    let FunDecl { name = declname; typesig = (return_type, param_types) } = decl in
    if name = declname && List.length params = List.length param_types then (
      let params = List.map2 (fun param ty ->
          string_of_type ty ^ " " ^ local (name_of_var param)
        ) params param_types
      in
      printf "define %s %s(%s)"
        (string_of_type return_type) (global name) (String.concat ", " params)
    ) else (
      failwith "Function signature mismatch"
    )
  | _, None -> failwith "Missing parameter list"

let emit_function_declaration (decl : IR.decl) =
  let FunDecl { name; typesig = (return_type, param_types) } = decl in
  let return = string_of_type return_type in
  let params = List.map string_of_type param_types in
    printf "declare %s %s(%s)\n"
      return (global name) (String.concat ", " params)

let emit_function (decl : IR.decl) (graph : Cfg.t) =
  (* Unnamed temporaries are numbered %0, %1, etc., assuming that all function
   * parameters are named and all basic blocks are labeled. *)
  gen_temp := Three_address_code.Utils.gen_name "%" 0;
  Cfg.get_first_basic_block graph
  |> Basic_block.entry_label
  |> emit_function_header decl;
  print_endline " {";
  emit_function_body decl graph;
  print_endline "}"

let emit ?(optimize = false) filename =
  let fundecl, fundef = Parse.parse_file filename in
  let basic_blocks = Basic_block.create_basic_blocks fundef in
  let graph = Cfg.construct basic_blocks in
  let ssa = Ssa.construct graph in
  emit_function
    (Option.get fundecl)
    (if optimize then Optim.optimize graph ssa else graph)
