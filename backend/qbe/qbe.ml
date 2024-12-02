open Three_address_code
open Three_address_code__IR
open Three_address_code__Utils
open Control_flow

let string_of_type = function
  | Type.Int -> "w"
  | Type.Void -> ""
  | Type.Ptr _ -> "l"

let string_of_binop = function
  | Plus -> "w add"
  | Minus -> "w sub"
  | Mul -> "w mul"
  | Div -> "w div"
  | Mod -> "w rem"

let string_of_relop = function
  | EQ -> "w ceqw"
  | NE -> "w cnew"
  | LT -> "w csltw"
  | GT -> "w csgtw"
  | LE -> "w cslew"
  | GE -> "w csgew"

let global name = "$" ^ name

let regexp = Str.regexp "^\\$"

let local name =
  (* '$' is reserved for global symbols *)
  "%" ^ Str.replace_first regexp "." name

let rec string_of_expr = function
  | Const n -> string_of_int n
  | Val (Var x) -> local x
  | Binop (op, e1, e2) ->
    string_of_binop op ^ " " ^ string_of_expr e1 ^ ", " ^ string_of_expr e2
  | Relop (op, e1, e2) ->
    string_of_relop op ^ " " ^ string_of_expr e1 ^ ", " ^ string_of_expr e2

let gen_temp = ref (Three_address_code.Utils.gen_name "%." 0)

let emit_basic_block (block : Basic_block.t) =
  let indent = 2 in
  let emit stmt =
    match !stmt with
    | Move (Var x, (Const _ as y))
    | Move (Var x, (Val _ as y)) ->
      printf ~indent "%s = w copy %s\n"
        (local x) (string_of_expr y)
    | Move (Var x, e) ->
      printf ~indent "%s = %s\n"
        (local x) (string_of_expr e)
    | Load (Var x, Mem (Var b, o)) ->
      let tmp1 = !gen_temp () in
      let tmp2 = !gen_temp () in
      printf ~indent "%s = l extsw %s\n"
        tmp1 (string_of_expr o);
      printf ~indent "%s = l add %s, %s\n"
        tmp2 (local b) tmp1;
      printf ~indent "%s = w loadw %s\n"
        (local x) tmp2
    | Store (Mem (Var b, o), e) ->
      let tmp1 = !gen_temp () in
      let tmp2 = !gen_temp () in
      printf ~indent "%s = l extsw %s\n"
        tmp1 (string_of_expr o);
      printf ~indent "%s = l add %s, %s\n"
        tmp2 (local b) tmp1;
      printf ~indent "storew %s, %s\n"
        (string_of_expr e) tmp2
    | Label (name, _) ->
      printf "%s\n"
        ("@" ^ name)
    | Jump (label, _) ->
      printf ~indent "jmp %s\n"
        ("@" ^ label)
    | Cond (Binop _ as e, (iftrue, _), (iffalse, _))
    | Cond (Relop _ as e, (iftrue, _), (iffalse, _)) ->
      let tmp = !gen_temp () in
      printf ~indent "%s = %s\n"
        tmp (string_of_expr e);
      printf ~indent "jnz %s, %s, %s\n"
        tmp ("@" ^ iftrue) ("@" ^ iffalse)
    | Cond (e, (iftrue, _), (iffalse, _)) ->
      printf ~indent "jnz %s, %s, %s\n"
        (string_of_expr e) ("@" ^ iftrue) ("@" ^ iffalse)
    | Return (Some e) ->
      printf ~indent "ret %s\n"
        (string_of_expr e)
    | Return None ->
      printf ~indent "ret\n"
    | Phi (x, xs) ->
      let args = List.map2 (fun (Var x) pred ->
          let label, _ = Basic_block.entry_label pred in
          Printf.sprintf "%s %s" ("@" ^ label) (local x)
        ) xs block.pred
      in
      printf ~indent "%s = w phi %s\n"
        (local (name_of_var x)) (String.concat ", " args)
  in
  List.iter emit block.stmts

let emit_function_body (graph : Cfg.t) =
  Cfg.iter (fun { block; _ } ->
      emit_basic_block block
    ) graph

let emit_function_header (decl : IR.decl) = function
  | name, Some params ->
    let FunDecl { name = declname; typesig = (return_type, param_types) } = decl in
    if name = declname && List.length params = List.length param_types then (
      let params = List.map2 (fun param ty ->
          string_of_type ty ^ " " ^ local (name_of_var param)
        ) params param_types
      in
      printf "function %s %s(%s)"
        (string_of_type return_type) (global name) (String.concat ", " params)
    ) else (
      failwith "Function signature mismatch"
    )
  | _, None -> failwith "Missing parameter list"

let emit_function (decl : IR.decl) (graph : Cfg.t) =
  gen_temp := Three_address_code.Utils.gen_name "%." 0;
  (* External linkage *)
  print_endline "export";
  Cfg.get_first_basic_block graph
  |> Basic_block.entry_label
  |> emit_function_header decl;
  print_endline " {";
  emit_function_body graph;
  print_endline "}"

let emit ?(optimize = false) filename =
  let fundecl, fundef = Parse.parse_file filename in
  let basic_blocks = Basic_block.create_basic_blocks fundef in
  let graph = Cfg.construct basic_blocks in
  let ssa = Ssa.construct graph in
  emit_function
    (Option.get fundecl)
    (if optimize then Optim.optimize graph ssa else graph)
