open Three_address_code__IR
open Control_flow

type fun_decl = {
  name : string;
  type_sig : ty * ty list;
}

and ty = Word | Long | Void

(* Constructor for function declarations *)
let declare ?(return = Void) name ~params =
  { name; type_sig = (return, params); }

let string_of_ty = function
  | Word -> "w"
  | Long -> "l"
  | Void -> ""

let global name = "$" ^ name

let regexp = Str.regexp "^\\$"

let local name =
  (* '$' is reserved for global symbols *)
  "%" ^ Str.replace_first regexp "." name

let get_first_basic_block (graph : Cfg.t) =
  let open Cfg in
  let entry = get_entry_node graph in
  assert (NodeSet.cardinal entry.succ = 1);
  (NodeSet.choose entry.succ).block

let printf ?(indent = 0) =
  print_string (String.make indent ' ');
  Printf.printf

let emit_function_header (block : Basic_block.t) (decl : fun_decl) =
  match Basic_block.entry_label block with
  | _, Some params ->
    let ty, tys = decl.type_sig in
    if List.length params = List.length tys then (
      let params = List.map2 (fun param ty ->
          string_of_ty ty ^ " " ^ local (name_of_var param)
        ) params tys
      in
      printf "function %s %s(%s)"
        (string_of_ty ty) (global decl.name) (String.concat ", " params)
    ) else (
      failwith "Function signature mismatch"
    )
  | _, None -> failwith "Missing parameter list"

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

let rec string_of_expr = function
  | Const n -> string_of_int n
  | Val (Var x) -> local x
  | Binop (op, e1, e2) ->
    string_of_binop op ^ " " ^ string_of_expr e1 ^ ", " ^ string_of_expr e2
  | Relop (op, e1, e2) ->
    string_of_relop op ^ " " ^ string_of_expr e1 ^ ", " ^ string_of_expr e2

let gen_temp = ref (Three_address_code__Utils.gen_name "%." 0)

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
        (string_of_expr e) (local iftrue) (local iffalse)
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

let emit_function (graph : Cfg.t) (decl : fun_decl) =
  gen_temp := Three_address_code__Utils.gen_name "%." 0;
  (* External linkage *)
  print_endline "export";
  emit_function_header (get_first_basic_block graph) decl;
  print_endline " {";
  emit_function_body graph;
  print_endline "}"

module Test = struct
  let fib graph =
    declare "fib"
      ~return:Word
      ~params:[Word]
    |> emit_function graph

  let pow graph =
    declare "pow"
      ~return:Word
      ~params:[Word; Word]
    |> emit_function graph

  let fastpow graph =
    declare "fastpow"
      ~return:Word
      ~params:[Word; Word]
    |> emit_function graph

  let sort graph =
    declare "sort"
      ~params:[Long; Word]
    |> emit_function graph

  let test01 graph =
    declare "test01"
      ~return:Word
      ~params:[Word; Word]
    |> emit_function graph

  let test02 graph =
    declare "test02"
      ~return:Word
      ~params:[]
    |> emit_function graph

  let test03 graph =
    declare "test03"
      ~params:[Word]
    |> emit_function graph

  let test04 graph =
    declare "test04"
      ~params:[Long; Word]
    |> emit_function graph

  let test05 graph =
    declare "test05"
      ~return:Word
      ~params:[]
    |> emit_function graph

  let test06 graph =
    declare "test06"
      ~return:Word
      ~params:[Word]
    |> emit_function graph

  let test07 graph =
    declare "test07"
      ~return:Word
      ~params:[Word]
    |> emit_function graph

  module M = Map.Make (String)

  let tests =
    M.empty
    |> M.add "fib"     fib
    |> M.add "pow"     pow
    |> M.add "fastpow" fastpow
    |> M.add "sort"    sort
    |> M.add "test01"  test01
    |> M.add "test02"  test02
    |> M.add "test03"  test03
    |> M.add "test04"  test04
    |> M.add "test05"  test05
    |> M.add "test06"  test06
    |> M.add "test07"  test07

  let emit ?(optimize = false) name =
    let open Graphs in
    match M.find_opt name tests with
    | Some test ->
      let graph = graph_of_input ("examples/" ^ name ^ ".hir") in
      let ssa = Ssa.construct graph in
      test (if optimize then Optim.optimize graph ssa else graph)
    | None -> failwith (name ^ " unknown")
end
