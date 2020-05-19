open Three_address_code__IR
open Basic
open Control_flow

module S = Set.Make (struct
  type t = var
  let compare = Stdlib.compare
end)

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

let phi_functions label jumps =
  match label with
  | Label (name, Some params) ->
    let rec loop i phis = function
      | x :: xs ->
        let xs' = List.map (function
            | Jump (name', Some xs')
            | Cond (_, (name', Some xs')) -> (
                assert (name' = name);
                List.nth xs' i
              )
            | _ -> failwith "phi_functions"
          ) jumps
        in
        loop (i + 1) (Phi (x, xs') :: phis) xs
      | [] -> List.rev phis
    in
    loop 0 [] params
  | _ -> failwith "phi_functions"

let insert_phi_functions_block (Basic_block (name, source_info) as block) ~pred =
  match source_info with
  | Some ({ stmts; _ } as info) -> (
      assert (stmts <> []);
      match List.hd stmts with
      | Label (l, _) as label ->
        let jumps = List.fold_left (fun jumps (Basic_block (_, source_info)) ->
            match source_info with
            | Some { stmts; _ } -> (
                match List.hd (List.rev stmts) with
                | Jump _ | Cond _ as jump -> jump :: jumps
                | _ -> jumps
              )
            | None -> jumps
          ) [] pred |> List.rev
        in
        if jumps = [] then (
          assert (name = "B1");
          assert (List.length pred = 1);
          let Basic_block (name, _) = List.hd pred in
          assert (name = "Entry");
          block
        ) else (
          let phi_funcs = phi_functions label jumps in
          Basic.create name ~source_info:
            (* Insert phi-functions and erase label parameters *)
            { info with stmts = Label (l, None) :: phi_funcs @ List.tl stmts }
        )
      | _ -> block
    )
  | None ->
    assert (name = "Entry" || name = "Exit");
    block

let remove_label_params block = function
  | Label (l, Some _) when block <> "B1" -> Label (l, None)
  | Jump (l, Some _) -> Jump (l, None)
  | Cond (e, (l, Some _)) -> Cond (e, (l, None))
  | s -> s

let remove_label_params_block (Basic_block (name, source_info) as block) =
  match source_info with
  | Some ({ stmts; _ } as info) ->
    Basic.create name ~source_info:
      { info with stmts = List.map (remove_label_params name) stmts }
  | None ->
    assert (name = "Entry" || name = "Exit");
    block

let minimize_block (Basic_block (name, source_info) as block) =
  let rec remove_phi_functions = function
    | (Label _ as label) :: stmts ->
      label :: remove_phi_functions stmts
    | Phi (x, [x']) :: stmts ->
      (* Replace x := PHI(x') with x := x' and perform copy propagation *)
      remove_phi_functions (
        Optim.propagate (Move (x, Ref x')) stmts
      )
    | (Phi (x, xs) as phi) :: stmts ->
      let xs = S.of_list xs in
      if S.cardinal xs = 1 && not (S.mem x xs) ||
         S.cardinal xs = 2 && S.mem x xs then
        (* Replace x := PHI(x, x') or x := PHI(x', x') with x := x' and perform
         * copy propagation *)
        let x' = S.find_first (( <> ) x) xs in
        remove_phi_functions (
          Optim.propagate (Move (x, Ref x')) stmts
        )
      else
        (* Keep this phi-function *)
        phi :: remove_phi_functions stmts
    | stmts -> stmts
  in
  match source_info with
  | Some ({ stmts; _ } as info) ->
    Basic.create name ~source_info: {
      info with stmts = remove_phi_functions stmts
    }
  | None ->
    assert (name = "Entry" || name = "Exit");
    block

(* Roughly follows the simple generation of SSA form by Aycock and Horspool:
 * (1) Insert phi-functions "everywhere" (the "really crude" approach)
 * (2) Delete unnecessary phi-functions, optimize, and repeat
 * The result is minimal, or close to minimal, SSA form *)
let insert_phi_functions graph =
  let open Cfg.Node in
  let open Cfg.NodeSet in
  (* Insert phi-functions *)
  Array.iter (fun node ->
      let entry = 0 in
      let exit = Array.length graph - 1 in
      if node.index <> entry && node.index <> exit then
        let pred = List.map (fun node -> node.block) (elements node.pred) in
        (* Insert phi-functions for every labeled jump (not only at join
         * points, where two or more control flow paths merge) *)
        if List.length pred > 0 then
          node.block <- insert_phi_functions_block node.block ~pred
    ) graph;
  (* Remove label parameters *)
  Array.iter (fun node ->
      node.block <- remove_label_params_block node.block
    ) graph;
  (* Delete unnecessary phi-functions *)
  Array.iter (fun node ->
      node.block <- minimize_block node.block
    ) graph
