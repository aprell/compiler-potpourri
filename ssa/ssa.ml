open Three_address_code__IR
open Basic_block
open Control_flow

module S = Liveness.Set

let parameterize_labels graph =
  let open Cfg.Node in

  (* Collect all variables that are live on entry to _some_ basic block
   * (see semi-pruned SSA form) *)
  let variables = Array.fold_left (fun non_locals { block; _ } ->
      let use, _ = Liveness.compute block in
      S.union use non_locals
    ) S.empty graph
  |> S.elements
  in

  let parameterize block =
    let rec loop acc = function
      | stmt :: stmts ->
        let stmt' = match !stmt with
          | Label (l, None) -> Label (l, Some variables)
          | Jump (l, None) -> Jump (l, Some variables)
          | Cond (e, (l1, None), (l2, None)) ->
            Cond (e, (l1, Some variables), (l2, Some variables))
          | _ -> !stmt
        in
        loop (stmt' :: acc) stmts
      | [] -> List.rev acc
    in
    match block.source with
    | Some { stmts; _ } ->
      let stmts = loop [] stmts in
      Basic_block.update block ~stmts:(List.map ref stmts)
    | None ->
      assert (block.name = "Entry" || block.name = "Exit");
      block
  in

  Array.iter (fun node ->
        node.block <- parameterize node.block
    ) graph

let rename_variables graph =
  let open Cfg.Node in

  let gen_name pref init =
    let count = ref init in
    fun ~bump ->
      if bump then incr count;
      Printf.sprintf "%s_%d" pref !count
  in

  let versions = Hashtbl.create 10 in

  let rename_variable (Var x) ~bump =
    match Hashtbl.find_opt versions x with
    | Some version -> Var (version ~bump)
    | None ->
      let version = gen_name x (-1) in
      Hashtbl.add versions x version;
      Var (version ~bump)
  in

  let rec rename_variables_expr = function
    | Const _ as e -> e
    | Ref x -> Ref (rename_variable x ~bump:false)
    | Binop (op, e1, e2) ->
      let e1' = rename_variables_expr e1 in
      let e2' = rename_variables_expr e2 in
      Binop (op, e1', e2')
    | Relop (op, e1, e2) ->
      let e1' = rename_variables_expr e1 in
      let e2' = rename_variables_expr e2 in
      Relop (op, e1', e2')
  in

  let rename_variables_stmt stmt =
    match !stmt with
    | Move (x, e) ->
      (* Evaluation order matters *)
      let e' = rename_variables_expr e in
      let x' = rename_variable x ~bump:true in
      Move (x', e')
    | Load (x, Mem { base; offset; }) ->
      let offset' = rename_variables_expr offset in
      let x' = rename_variable x ~bump:true in
      Load (x', Mem { base; offset = offset' })
    | Store (Mem { base; offset; }, e) ->
      let offset' = rename_variables_expr offset in
      let e' = rename_variables_expr e in
      Store (Mem { base; offset = offset' }, e')
    | Label (l, Some xs) ->
      let xs' = List.map (rename_variable ~bump:true) xs in
      Label (l, Some xs')
    | Label (_, None) as s -> s
    | Jump (l, Some xs) ->
      let xs' = List.map (rename_variable ~bump:false) xs in
      Jump (l, Some xs')
    | Jump (_, None) as s -> s
    | Cond (e, (l1, Some xs), (l2, Some ys)) ->
      let e' = rename_variables_expr e in
      let xs' = List.map (rename_variable ~bump:false) xs in
      let ys' = List.map (rename_variable ~bump:false) ys in
      Cond (e', (l1, Some xs'), (l2, Some ys'))
    | Cond (e, then_, else_) ->
      let e' = rename_variables_expr e in
      Cond (e', then_, else_)
    | Receive x ->
      let x' = rename_variable x ~bump:true in
      Receive x'
    | Return (Some e) ->
      let e' = rename_variables_expr e in
      Return (Some e')
    | Return None as s -> s
    | _ -> assert false
  in

  let rename block =
    match block.source with
    | Some { stmts; _ } ->
      let stmts = List.map rename_variables_stmt stmts in
      Basic_block.update block ~stmts:(List.map ref stmts)
    | None ->
      assert (block.name = "Entry" || block.name = "Exit");
      block
  in

  Array.iter (fun node ->
      node.block <- rename node.block
    ) graph

let create_phi_functions label jumps =
  match label with
  | Label (name, Some params) ->
    let rec loop i phi_funcs = function
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
            | _ -> invalid_arg "create_phi_functions"
          ) jumps
        in
        loop (i + 1) (Phi (x, xs') :: phi_funcs) xs
      | [] -> List.rev phi_funcs
    in
    loop 0 [] params
  | _ -> invalid_arg "create_phi_functions"

let insert_phi_functions graph =
  let open Cfg.Node in
  let open Cfg.NodeSet in

  let insert block ~pred =
    match block.source with
    | Some { stmts; _ } -> (
        assert (stmts <> []);
        match !(List.hd stmts) with
        | Label (l, _) as label ->
          let jumps = List.fold_left (fun jumps block ->
              match block.source with
              | Some { stmts; _ } -> (
                  match !(List.(hd (rev stmts))) with
                  | Jump _ | Cond _ as jump -> jump :: jumps
                  | _ -> assert false
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
            let phi_funcs = create_phi_functions label jumps in
            Basic_block.update block ~stmts:
              (* Insert phi-functions and erase label parameters *)
              (List.map ref (Label (l, None) :: phi_funcs) @ List.tl stmts)
          )
        | _ -> assert false
      )
    | None ->
      assert false
  in

  let erase_label_params block =
    match block.source with
    | Some { stmts; _ } ->
      let tl = List.(hd (rev stmts)) in (
        match !tl with
        | Jump (l, Some _) ->
          tl := Jump (l, None)
        | Cond (e, (l1, Some _), (l2, Some _)) ->
          tl := Cond (e, (l1, None), (l2, None))
        | _ -> ()
      )
    | None ->
      assert (block.name = "Entry" || block.name = "Exit")
  in

  Array.iter (fun node ->
      if node.block.name <> "Entry" && node.block.name <> "Exit" then
        let pred = List.map (fun node -> node.block) (elements node.pred) in
        (* Insert phi-functions for every labeled jump (not only at join
         * points, where two or more control flow paths merge) *)
        if List.length pred > 0 then
          node.block <- insert node.block ~pred
    ) graph;

  (* Erase remaining label parameters and build def-use chains *)
  Array.iter (fun { block; _ } ->
      erase_label_params block;
      Def_use_chain.build block
   ) graph

let minimize_phi_functions graph =
  let open Cfg.Node in

  let worklist = Queue.create () in

  (* TODO: This is a hack and requires a better solution *)
  let node_of_block block =
    let i = int_of_string (String.(sub block.name 1 (length block.name - 1))) in
    graph.(i)
  in

  let rec remove_phi_functions block =
    let rec loop = function
      | stmt :: stmts -> (
          match !stmt with
          | Label _ ->
            stmt :: loop stmts
          | Phi (x, [x']) -> (
              (* Replace x := PHI(x') with x := x' and perform copy propagation *)
              List.iter (fun block' ->
                  if !block'.name <> block.name then
                    Queue.add (fun () ->
                        let node = node_of_block !block' in
                        node.block <- remove_phi_functions !block'
                      ) worklist
                ) (Def_use_chain.basic_blocks_of_uses x);
              Optim.propagate (Move (x, Ref x'));
              loop stmts
            )
          | Phi (x, xs) -> (
              let xs = S.of_list xs in
              if S.cardinal xs = 1 && not (S.mem x xs) ||
                 S.cardinal xs = 2 && S.mem x xs then
                (* Replace x := PHI(x, x') or x := PHI(x', x') with x := x' and
                 * perform copy propagation *)
                let x' = S.find_first (( <> ) x) xs in
                List.iter (fun block' ->
                    if !block'.name <> block.name then
                      Queue.add (fun () ->
                          let node = node_of_block !block' in
                          node.block <- remove_phi_functions !block'
                        ) worklist
                  ) (Def_use_chain.basic_blocks_of_uses x);
                Optim.propagate (Move (x, Ref x'));
                loop stmts
              else
                (* Keep this phi-function *)
                stmt :: loop stmts
            )
          | _ -> stmt :: loop stmts
        )
      | stmts -> stmts
    in
    match block.source with
    | Some { stmts; _ } ->
        Basic_block.update block ~stmts:(loop stmts)
    | None ->
      assert (block.name = "Entry" || block.name = "Exit");
      block
  in

  (* Seed work list *)
  Array.iter (fun node ->
      Queue.add (fun () ->
          node.block <- remove_phi_functions node.block
        ) worklist
    ) graph;

  (* Iterate *)
  while not (Queue.is_empty worklist) do
    Queue.take worklist ()
  done

(* Roughly follows the simple generation of SSA form by Aycock and Horspool:
 * (1) Insert phi-functions "everywhere" (the "really crude" approach)
 * (2) Delete unnecessary phi-functions, optimize, and repeat
 * The result is minimal, or close to minimal, SSA form *)
let convert_to_ssa graph =
  parameterize_labels graph;
  rename_variables graph;
  insert_phi_functions graph;
  minimize_phi_functions graph
