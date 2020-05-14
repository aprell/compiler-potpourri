open Three_address_code__IR
open Basic
open Control_flow

let parameterize_labels_block (Basic_block (name, source_info) as block) ~variables =
  match source_info with
  | Some ({ stmts; _ } as info) ->
    let rec loop acc = function
      | stmt :: stmts ->
        let stmt' = match stmt with
          | Label (l, None) -> Label (l, Some variables)
          | Jump (l, None) -> Jump (l, Some variables)
          | Cond (e, (l, None)) -> Cond (e, (l, Some variables))
          | _ -> stmt
        in
        loop (stmt' :: acc) stmts
      | [] -> List.rev acc
    in
    Basic.create name ~source_info:
      { info with stmts = loop [] stmts }
  | None ->
    assert (name = "Entry" || name = "Exit");
    block

let parameterize_labels graph =
  let open Cfg.Node in
  let variables = Array.map (fun { block = Basic_block (name, source_info); _ } ->
      match source_info with
      | Some { stmts; _ } -> stmts
      | None -> assert (name = "Entry" || name = "Exit"); []
    ) graph |> Array.to_list |> List.flatten |> all_variables
  in
  Array.iter (fun node ->
      node.block <- parameterize_labels_block node.block ~variables
    ) graph

let gen_sym pref init =
  let count = ref init in
  fun ~bump ->
    if bump then incr count;
    let c = !count in
    pref ^ "_" ^ string_of_int c

let generation_counts = Hashtbl.create 10

let rename_variable (Var x) ~bump =
  match Hashtbl.find_opt generation_counts x with
  | Some gen_name -> Var (gen_name ~bump)
  | None ->
    let gen_name = gen_sym x (-1) in
    Hashtbl.add generation_counts x gen_name;
    Var (gen_name ~bump)

let rename_variables_label ((name, params) as label) ~bump =
  match params with
  | Some xs ->
    let xs' = List.map (rename_variable ~bump) xs in
    (name, Some xs')
  | None -> label

let rec rename_variables_expr = function
  | Const _ as e -> e
  | Ref x -> Ref (rename_variable x ~bump:false)
  | Binop (op, e1, e2) ->
    Binop (op, rename_variables_expr e1, rename_variables_expr e2)
  | Relop (op, e1, e2) ->
    Relop (op, rename_variables_expr e1, rename_variables_expr e2)

let rename_variables_stmt = function
  | Move (x, e) ->
    Move (rename_variable x ~bump:true, rename_variables_expr e)
  | Load (x, Mem { base; offset; }) ->
    Load (rename_variable x ~bump:true, Mem { base; offset = rename_variables_expr offset })
  | Store (Mem { base; offset; }, e) ->
    Store (Mem { base; offset = rename_variables_expr offset }, rename_variables_expr e)
  | Label l ->
    Label (rename_variables_label l ~bump:true)
  | Jump l ->
    Jump (rename_variables_label l ~bump:false)
  | Cond (e, l) ->
    Cond (rename_variables_expr e, rename_variables_label l ~bump:false)
  | Receive x ->
    Receive (rename_variable x ~bump:true)
  | Return (Some e) ->
    Return (Some (rename_variables_expr e))
  | Return None as s -> s
  | _ -> assert false

let rename_variables_block (Basic_block (name, source_info) as block) =
  match source_info with
  | Some ({ stmts; _ } as info) ->
    Basic.create name ~source_info:
      { info with stmts = List.map rename_variables_stmt stmts }
  | None ->
    assert (name = "Entry" || name = "Exit");
    block

let rename_variables graph =
  let open Cfg.Node in
  Array.iter (fun node ->
      node.block <- rename_variables_block node.block
    ) graph
