open Three_address_code__IR
open Basic_block
open Control_flow

module S = Set.Make (struct
  type t = var
  let compare = Stdlib.compare
end)

let parameterize_labels_block block ~variables =
  match block.source with
  | Some { stmts; _ } ->
    let rec loop acc = function
      | stmt :: stmts ->
        let stmt' = match stmt with
          | Label (l, None) -> Label (l, Some variables)
          | Jump (l, None) -> Jump (l, Some variables)
          | Cond (e, (l1, None), (l2, None)) ->
            Cond (e, (l1, Some variables), (l2, Some variables))
          | _ -> stmt
        in
        loop (stmt' :: acc) stmts
      | [] -> List.rev acc
    in
    Basic_block.update block ~stmts:
      (loop [] stmts)
  | None ->
    assert (block.name = "Entry" || block.name = "Exit");
    block

let parameterize_labels graph =
  let open Cfg.Node in
  (* Collect all variables that are live on entry to _some_ basic block
   * (see semi-pruned SSA form) *)
  let variables = Array.fold_left (fun non_locals { block; _ } ->
      match block.source with
      | Some { use; _ } ->
        S.union (S.of_list use) non_locals
      | None ->
        assert (block.name = "Entry" || block.name = "Exit");
        non_locals
    ) S.empty graph
  |> S.elements
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
  | Cond (e, l1, l2) ->
    Cond (rename_variables_expr e,
          rename_variables_label l1 ~bump:false,
          rename_variables_label l2 ~bump:false)
  | Receive x ->
    Receive (rename_variable x ~bump:true)
  | Return (Some e) ->
    Return (Some (rename_variables_expr e))
  | Return None as s -> s
  | _ -> assert false

let rename_variables_block block =
  match block.source with
  | Some { stmts; _ } ->
    Basic_block.update block ~stmts:
      (List.map rename_variables_stmt stmts)
  | None ->
    assert (block.name = "Entry" || block.name = "Exit");
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
            | Jump (name', Some xs') ->
              assert (name' = name);
              List.nth xs' i
            | Cond (_, (name', Some xs'), (name'', Some xs'')) ->
              if (name' = name) then (
                List.nth xs' i
              ) else (
                assert (name'' = name);
                List.nth xs'' i
              )
            | _ -> failwith "phi_functions"
          ) jumps
        in
        loop (i + 1) (Phi (x, xs') :: phis) xs
      | [] -> List.rev phis
    in
    loop 0 [] params
  | _ -> failwith "phi_functions"

let insert_phi_functions_block block ~pred =
  match block.source with
  | Some { stmts; _ } -> (
      assert (stmts <> []);
      match List.hd stmts with
      | Label (l, _) as label ->
        let jumps = List.fold_left (fun jumps block ->
            match block.source with
            | Some { stmts; _ } -> (
                match List.hd (List.rev stmts) with
                | Jump _ | Cond _ as jump -> jump :: jumps
                | _ -> jumps
              )
            | None -> jumps
          ) [] pred |> List.rev
        in
        if jumps = [] then (
          assert (block.name = "B1");
          assert (List.length pred = 1);
          assert ((List.hd pred).name = "Entry");
          block
        ) else (
          let phi_funcs = phi_functions label jumps in
          Basic_block.update block ~stmts:
            (* Insert phi-functions and erase label parameters *)
            (Label (l, None) :: phi_funcs @ List.tl stmts)
        )
      | _ -> block
    )
  | None ->
    assert (block.name = "Entry" || block.name = "Exit");
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
    ) graph

let remove_label_params = function
  | Jump (l, Some _) -> Jump (l, None)
  | Cond (e, (l1, Some _), (l2, Some _)) -> Cond (e, (l1, None), (l2, None))
  | s -> s

let minimize_phi_functions graph =
  let open Cfg in
  let open Cfg.Node in

  let worklist = Queue.create () in
  let pending = Queue.create () in

  let copy_propagate move stmts =
    let x, y = match move with
      | Move (x, Ref y) -> x, y
      | _ -> invalid_arg "copy_propagate"
    in
    let rec loop acc = function
      | [Jump (_, Some xs) as jump]
      | [Cond (_, (_, Some xs), _) as jump] ->
        if List.mem x xs then Queue.add move pending;
        List.rev (Optim.replace_stmt x y jump :: acc)
      | [Return _ as ret] ->
        List.rev (Optim.replace_stmt x y ret :: acc)
      | stmt :: stmts ->
        loop (Optim.replace_stmt x y stmt :: acc) stmts
      | [] ->
        (* Every (reachable) basic block ends with a jump or return *)
        assert false
    in
    loop [] stmts
  in

  let rec remove_phi_functions ?propagate { block; succ; _ } =
    let rec loop = function
      | (Label _ as label) :: stmts ->
        label :: loop stmts
      | Phi (x, [x']) :: stmts ->
        (* Replace x := PHI(x') with x := x' and perform copy propagation *)
        loop (copy_propagate (Move (x, Ref x')) stmts)
      | (Phi (x, xs) as phi) :: stmts ->
        let xs = S.of_list xs in
        if S.cardinal xs = 1 && not (S.mem x xs) ||
           S.cardinal xs = 2 && S.mem x xs then
          (* Replace x := PHI(x, x') or x := PHI(x', x') with x := x' and perform
           * copy propagation *)
          let x' = S.find_first (( <> ) x) xs in
          loop (copy_propagate (Move (x, Ref x')) stmts)
        else
          (* Keep this phi-function *)
          phi :: loop stmts
      | stmts -> stmts
    in
    assert (Queue.is_empty pending);
    match block.source with
    | Some { stmts; _ } -> (
        let stmts = loop (
            match propagate with
            | Some moves ->
              Queue.fold (Fun.flip copy_propagate) stmts moves
            | None ->
              stmts
          )
        in
        if not (Queue.is_empty pending) then (
          NodeSet.iter (fun s ->
              let propagate = Queue.copy pending in
              Queue.add (fun () ->
                  s.block <- remove_phi_functions s ~propagate
                ) worklist
            ) succ;
          Queue.clear pending
        );
        Basic_block.update block ~stmts
      )
    | None ->
      assert (block.name = "Entry" || block.name = "Exit");
      block
  in

  (* Seed work list *)
  Array.iter (fun node ->
      Queue.add (fun () ->
          node.block <- remove_phi_functions node
        ) worklist;
    ) graph;

  (* Iterate *)
  while not (Queue.is_empty worklist) do
    Queue.take worklist ()
  done;

  (* Remove remaining label parameters *)
  Array.iter (fun node ->
      match node.block.source with
      | Some { stmts; _ } ->
        node.block <- Basic_block.update node.block ~stmts:
            (List.map remove_label_params stmts)
      | None ->
        assert (node.block.name = "Entry" || node.block.name = "Exit");
    ) graph
