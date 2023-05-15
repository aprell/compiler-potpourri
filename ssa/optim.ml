open Three_address_code__IR
open Three_address_code__Utils
open Control_flow

let remove_def x ssa_graph =
  let block, stmt = Option.get (Ssa.Graph.get_def x ssa_graph) in
  !block.stmts <- List.filter (( <> ) !stmt) !block.stmts;
  Ssa.Graph.remove_def x ssa_graph

let propagate_constant stmt ssa_graph =
  match stmt with
  | Move (x, Const n) ->
    let uses = Ssa.Graph.get_uses x ssa_graph in
    let phis, rest = List.partition (fun (_, stmt) ->
        is_phi !(!stmt)
      ) uses
    in
    List.iter (fun (_, stmt) ->
        replace ~stmt x (Const n)
      ) rest;
    if phis = [] then remove_def x ssa_graph
    else Ssa.Graph.set_uses x phis ssa_graph
  | _ -> invalid_arg "propagate_constant"

let propagate_copy stmt ssa_graph =
  match stmt with
  | Move (x, Val y) ->
    let uses = Ssa.Graph.get_uses x ssa_graph in
    let phis, rest = List.partition (fun (_, stmt) ->
        is_phi !(!stmt)
      ) uses
    in
    List.iter (fun ((_, stmt) as use) ->
        replace ~stmt x (Val y);
        Ssa.Graph.add_use y use ssa_graph
      ) rest;
    if phis = [] then remove_def x ssa_graph
    else Ssa.Graph.set_uses x phis ssa_graph
  | _ -> invalid_arg "propagate_copy"

let can_optimize =
  List.exists (
    fun ((_, use), _) ->
      not (is_phi !(!use))
  )

let propagate_constants ?(dump = false) graph ssa_graph =
  Analysis.Sccp.(run !graph ssa_graph |> get_constants)
  |> List.iter (fun (x, n) ->
      if can_optimize (Ssa.Graph.get_use_def x ssa_graph) then (
        let stmt = Move (x, Const n) in
        propagate_constant stmt ssa_graph;
        if dump then (
          print_endline ("After propagating " ^ string_of_stmt stmt ^ ":");
          Ssa.Graph.print ssa_graph;
          print_newline ()
        )
      )
    );
  false

let propagate_copies ?(dump = false) ssa_graph =
  match (
    (* Find a copy to propagate *)
    Ssa.Graph.find_first (fun ((_, stmt), uses) ->
        match !(!stmt) with
        | Move (_, Val _)
          when can_optimize uses -> true
        | _ -> false
      ) ssa_graph
  ) with
  | Some (_, ((_, stmt), _)) ->
    propagate_copy !(!stmt) ssa_graph;
    if dump then (
      print_endline ("After propagating " ^ string_of_stmt !(!stmt) ^ ":");
      Ssa.Graph.print ssa_graph;
      print_newline ()
    );
    true
  | _ -> false

let eliminate_dead_code ?(dump = false) ssa_graph =
  match (
    (* Find a dead variable *)
    Ssa.Graph.find_first (fun ((_, def), uses) ->
        match !(!def) with
        | Label _ -> false
        | Phi _ -> List.map (fst >> snd) uses = [def]
        | _ -> uses = []
      ) ssa_graph
  ) with
  | Some (_, ((_, stmt), _)) -> (
      match !(!stmt) with
      | Move (x, _)
      | Load (x, _)
      | Phi (x, _) ->
        (* Assume no side effects *)
        remove_def x ssa_graph;
        if dump then (
          print_endline ("After removing " ^ string_of_stmt !(!stmt) ^ ":");
          Ssa.Graph.print ssa_graph;
          print_newline ()
        );
        true
      | _ -> assert false
    )
  | None -> false

let reachable { Basic_block.number; _ } graph =
  Option.is_some (Cfg.get_node_opt number graph)

let eliminate_unreachable_code ?(dump = false) graph ssa_graph =
  match (
    (* Find a definition that has become unreachable *)
    Ssa.Graph.find_first (fun ((block, _), _) ->
        not (reachable !block !graph)
      ) ssa_graph
  ) with
  | Some (x, ((_, stmt), uses)) ->
    List.iter (fun ((block, stmt), _) ->
        if reachable !block !graph then (
          match !(!stmt) with
          | Phi (xn, xs) -> (
              assert (List.mem x xs);
              match List.filter (( <> ) x) xs with
              | [x1] -> !stmt := Move (xn, Val x1)
              | xs ->
                let xs' = Vars.of_list xs in
                if Vars.cardinal xs' = 2 && Vars.mem xn xs' then
                  (* Replace x := PHI(x, x') with x := x' *)
                  let xn' = Vars.find_first (( <> ) xn) xs' in
                  !stmt := Move (xn, Val xn')
                else
                  !stmt := Phi (xn, xs)
            )
          | _ -> assert false
        )
      ) uses;
    Ssa.Graph.remove_def x ssa_graph;
    if dump then (
      print_endline ("After removing " ^ string_of_stmt !(!stmt) ^ ":");
      Ssa.Graph.print ssa_graph;
      print_newline ()
    );
    true
  | _ -> false

let remove_unreachable_nodes (graph : Cfg.t) : Cfg.t =
  let open Cfg in
  let reachable_nodes = NodeSet.of_list (dfs_reverse_postorder graph) in
  let reachable node = NodeSet.mem node reachable_nodes in
  filter (fun _ node ->
      if not (reachable node) then (
        NodeSet.iter (fun succ ->
            if (reachable succ) then
              succ.block.pred <- List.filter (fun pred ->
                  Basic_block.compare pred node.block <> 0
                ) succ.block.pred;
          ) node.succ;
        false
      ) else (
        assert (NodeSet.for_all reachable node.succ);
        node.pred <- NodeSet.filter reachable node.pred;
        true
      )
    ) graph

let remove_branch node ~label =
  let open Cfg in
  let target = NodeSet.filter (fun succ ->
      Basic_block.entry_label succ.block = label
    ) node.Node.succ
  in
  assert (NodeSet.cardinal target = 1);
  Node.(node =|> NodeSet.choose target)

let retarget_branch node ~label succ =
  let open Cfg in
  let label' = Basic_block.entry_label succ.Node.block in
  begin match Basic_block.last_stmt node.Node.block with
    | Some stmt -> (
        match !stmt with
        | Jump l when l = label ->
          stmt := Jump label';
          remove_branch node ~label
        | Cond (e, l1, l2) when l1 = label && l2 = label ->
          stmt := Cond (e, label', label');
          remove_branch node ~label
        | Cond (e, l1, l2) when l1 = label ->
          stmt := Cond (e, label', l2);
          remove_branch node ~label
        | Cond (e, l1, l2) when l2 = label ->
          stmt := Cond (e, l1, label');
          remove_branch node ~label
        | _ -> assert false
      )
    | None -> ()
  end;
  Node.(node => succ)

let simplify_control_flow ?(_dump = false) graph _ssa_graph =
  let open Cfg in
  let simplify_branch (node : Node.t) =
    match Basic_block.last_stmt node.block with
    | Some stmt -> (
        match !stmt with
        | Cond (Const 0, then_, else_) ->
          stmt := Jump else_;
          if then_ <> else_ then
            remove_branch node ~label:then_
        | Cond (Const 1, then_, else_) ->
          stmt := Jump then_;
          if else_ <> then_ then
            remove_branch node ~label:else_
        | _ -> ()
      )
    | None -> ()
  in

  let is_simple { Node.block = { stmts; _ }; _ } =
    List.length stmts = 2 &&
    match !(List.hd stmts), !(List.(hd (tl stmts))) with
    | Label (_, Some []), Jump (_, None)
    | Label (_, None), Jump (_, None)
    | Label (_, None), Return (Some (Const _))
    | Label (_, None), Return None -> true
    | _ -> false
  in

  let can_skip { Node.block = { stmts; _ }; _ } =
    List.length stmts = 2 &&
    match !(List.hd stmts), !(List.(hd (tl stmts))) with
    | Label (l1, None), Jump (l2, None) when l1 <> l2 -> true
    | _ -> false
  in

  let skip (node : Node.t) =
    let open Node in
    assert (NodeSet.cardinal node.succ = 1);
    let succ = NodeSet.choose node.succ in
    node =|> succ;
    NodeSet.iter (fun pred ->
        retarget_branch pred succ
          ~label:(Basic_block.entry_label node.block)
      ) node.pred
  in

  (* Guard against invalidating def-use information *)
  let can_combine (node : Node.t) =
    is_simple node && (
      assert (NodeSet.cardinal node.succ = 1);
      let succ = NodeSet.choose node.succ in
      succ != node && is_simple succ
    )
  in

  let rec combine_nodes (graph : t) =
    match List.find_opt can_combine (get_nodes graph) with
    | Some node ->
      assert (NodeSet.cardinal node.succ = 1);
      let succ = NodeSet.choose node.succ in
      graph
      |> remove_node node
      |> remove_node succ
      |> add_node (Node.combine node succ)
      |> combine_nodes
    | None ->
      graph
  in

  let simplify graph =
    iter (fun node ->
      simplify_branch node;
      if can_skip node then skip node
    ) graph;
    remove_unreachable_nodes graph
    |> combine_nodes
  in

  let graph' = simplify !graph in
  let simplified = not (Cfg.equal graph' !graph) in
  graph := graph';
  simplified

let optimize ?(dump = false) graph ssa_graph =
  let graph = ref graph in
  let changed = ref true in
  while !changed do
    changed := List.exists (( = ) true) [
        simplify_control_flow graph ssa_graph ~_dump:dump;
        eliminate_unreachable_code graph ssa_graph ~dump;
        eliminate_dead_code ssa_graph ~dump;
        propagate_constants graph ssa_graph ~dump;
        propagate_copies ssa_graph ~dump;
      ]
  done;
  !graph
