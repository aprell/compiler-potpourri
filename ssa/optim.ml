open Three_address_code__IR
open Three_address_code__Utils
open Control_flow

let remove_def x ssa_graph =
  let block, stmt = Option.get (Ssa.Graph.get_def x ssa_graph) in
  !block.stmts <- List.filter (( <> ) !stmt) !block.stmts;
  Ssa.Graph.remove_def x ssa_graph

let variables_in_use = function
  | Move (_, e)
  | Cond (e, _, _)
  | Return (Some e) -> collect_variables e
  | Load (_, Mem (b, e)) -> Vars.(add b (collect_variables e))
  | Store (Mem (b, e1), e2) -> Vars.(add b (union (collect_variables e1) (collect_variables e2)))
  | Phi (_, xs) -> Vars.of_list xs
  | _ -> Vars.empty

let propagate_constant stmt ssa_graph =
  match stmt with
  | Move (x, Const n) ->
    let uses = Ssa.Graph.get_uses x ssa_graph in
    let phis, rest = List.partition (fun (_, stmt) ->
        is_phi !(!stmt)
      ) uses
    in
    List.iter (fun ((_, stmt) as use) ->
        let xs = variables_in_use !(!stmt) in
        assert (Vars.mem x xs);
        replace ~stmt x (Const n);
        let ys = variables_in_use !(!stmt) in
        assert (not (Vars.mem x ys));
        Vars.(diff (remove x xs) ys) |> Vars.iter (fun y ->
            (* Remove use of variable y, y != x, that was optimized away *)
            Ssa.Graph.remove_use y use ssa_graph
          )
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
  if dump then print_endline "Propagating constants";
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

let propagate_copies ?(dump = false) _graph ssa_graph =
  if dump then print_endline "Propagating copies";
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

let eliminate_dead_code ?(dump = false) _graph ssa_graph =
  if dump then print_endline "Eliminating dead code";
  match (
    (* Find a dead variable *)
    Ssa.Graph.find_first (fun ((_, def), uses) ->
        match !(!def) with
        | Label _ -> false
        | Phi _ -> List.map (fst >> snd) uses = [def] || uses = []
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

let eliminate_unreachable_code ?(dump = false) graph ssa_graph =
  if dump then print_endline "Eliminating unreachable code";

  let open Cfg in
  let reachable_nodes = NodeSet.of_list (dfs_reverse_postorder !graph) in
  let reachable block = NodeSet.exists (fun node -> block == node.block) reachable_nodes in

  let eliminate_def x =
    let uses = Ssa.Graph.get_uses x ssa_graph in
    List.iter (fun (block, stmt) ->
        if reachable !block then (
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
    Ssa.Graph.remove_def x ssa_graph
  in

  let eliminate_use x use =
    Ssa.Graph.remove_use x use ssa_graph
  in

  let eliminate_unreachable block =
    let visit stmt =
      let eliminate_use' =
        Fun.flip eliminate_use (ref block, ref stmt)
      in
      match !stmt with
      | Move (x, _)
      | Load (x, _)
      | Phi (x, _) ->
        eliminate_def x;
        if dump then (
          print_endline ("After removing " ^ string_of_stmt !stmt ^ ":");
          Ssa.Graph.print ssa_graph;
          print_newline ()
        )
      | Store (Mem (b, Val o), e) ->
        eliminate_use' b;
        eliminate_use' o;
        collect_variables e |> Vars.iter eliminate_use'
      | Store (Mem (b, Const _), e) ->
        eliminate_use' b;
        collect_variables e |> Vars.iter eliminate_use'
      | Cond (e, _, _)
      | Return (Some e) ->
        collect_variables e |> Vars.iter eliminate_use'
      | _ -> ()
    in
    List.iter visit block.stmts
  in

  let reachable node = NodeSet.mem node reachable_nodes in
  let eliminated = ref false in

  graph := filter (fun _ node ->
      if not (reachable node) then (
        NodeSet.iter (fun succ ->
            if (reachable succ) then
              succ.block.pred <- List.filter (fun pred ->
                  Basic_block.compare pred node.block <> 0
                ) succ.block.pred;
          ) node.succ;
        eliminate_unreachable node.block;
        eliminated := true;
        if dump then print_endline ("Removed " ^ node.block.name);
        false
      ) else (
        assert (NodeSet.for_all reachable node.succ);
        node.pred <- NodeSet.filter reachable node.pred;
        true
      )
    ) !graph;
  !eliminated

let simplify_control_flow ?(dump = false) graph ssa_graph =
  if dump then print_endline "Simplifying control flow";

  let open Cfg in
  let simplified = ref false in

  let remove_branch node ~label =
    let target = NodeSet.filter (fun succ ->
        Basic_block.entry_label succ.block = label
      ) node.Node.succ
    in
    assert (NodeSet.cardinal target = 1);
    let target = NodeSet.choose target in
    let phis = List.filter (( ! ) >> is_phi) target.block.stmts in
    List.iter (fun phi ->
        match !phi with
        | Phi (x, xs) -> (
          let x' = List.(assoc node.block (combine target.block.pred xs)) in
          Ssa.Graph.remove_use x' (ref target.block, ref phi) ssa_graph;
          match List.filter (( <> ) x') xs with
          | [x'] -> phi := Move (x, Val x')
          | xs' -> phi := Phi (x, xs')
        )
        | _ -> assert false
      ) phis;
    Node.(node =|> target)
  in

  let retarget_branch node ~label succ =
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
  in

  let simplify_branch (node : Node.t) =
    match Basic_block.last_stmt node.block with
    | Some stmt -> (
        match !stmt with
        | Cond (Const 0, then_, else_) ->
          stmt := Jump else_;
          if then_ <> else_ then
            remove_branch node ~label:then_;
          true
        | Cond (Const 1, then_, else_) ->
          stmt := Jump then_;
          if else_ <> then_ then
            remove_branch node ~label:else_;
          true
        | Cond (e, then_, else_) when then_ = else_ ->
          Vars.iter (fun x ->
              Ssa.Graph.remove_use x (ref node.block, ref stmt) ssa_graph;
            ) (collect_variables e);
          stmt := Jump then_;
          true
        | Cond (e, then_, else_) (* when then_ <> else_ *) -> (
          match List.map (fun { Basic_block.stmts; _ } -> List.tl stmts) node.block.succ with
          | [stmts; stmts'] when stmts = stmts' ->
            Vars.iter (fun x ->
                Ssa.Graph.remove_use x (ref node.block, ref stmt) ssa_graph;
              ) (collect_variables e);
            stmt := Jump then_;
            remove_branch node ~label:else_;
            true
          | _ -> false
        )
        | _ -> false
      )
    | None -> false
  in

  let is_empty { Node.block = { stmts; _ }; _ } =
    List.length stmts = 2 &&
    match !(List.hd stmts), !(List.(hd (tl stmts))) with
    | Label (l1, None), Jump (l2, None) when l1 <> l2 -> true
    | _ -> false
  in

  (* We use a different definition than in cfg.ml to avoid invalidating
   * def-use information *)
  let is_simple { Node.block = { stmts; _ }; _ } =
    List.length stmts = 2 &&
    match !(List.hd stmts), !(List.(hd (tl stmts))) with
    | Label (_, Some []), Jump (_, None)
    | Label (_, None), Jump (_, None)
    | Label (_, None), Return (Some (Const _))
    | Label (_, None), Return None -> true
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
      ) node.pred;
    simplified := true;
    if dump then print_endline ("Skipped " ^ node.block.name)
  in

  let can_combine (node : Node.t) =
    is_simple node && NodeSet.cardinal node.succ = 1 && (
      let succ = NodeSet.choose node.succ in
      succ != node && is_simple succ
    )
  in

  let combine_nodes (graph : t) =
    match List.find_opt can_combine (get_nodes graph) with
    | Some node ->
      assert (NodeSet.cardinal node.succ = 1);
      let succ = NodeSet.choose node.succ in
      let graph' = graph
        |> remove_node node
        |> remove_node succ
        |> add_node (Node.combine node succ)
      in
      simplified := true;
      if dump then print_endline ("Combined " ^ node.block.name ^ " and " ^ succ.block.name);
      graph'
    | None ->
      graph
  in

  let simplify graph =
    iter (fun node ->
      simplified := simplify_branch node;
      if is_empty node then skip node
    ) graph;
    combine_nodes graph
  in

  graph := simplify !graph;
  !simplified

let check_phi_functions ?(dump = false) _graph ssa_graph =
  if dump then print_endline "Checking PHI functions";

  match (
    (* Look for inconsistent phi-functions *)
    Ssa.Graph.find_first (fun ((block, def), _) ->
        match !(!def) with
        | Phi (_, xs) -> List.(length xs <> length !block.pred)
        | _ -> false
      ) ssa_graph
  ) with
  | Some (_, ((block, stmt), _)) -> (
      match !(!stmt) with
      | Phi _ ->
        Printf.eprintf "Found inconsistent %s: %s has %d incoming edge(s)\n"
          (string_of_stmt !(!stmt)) !block.name (List.length !block.pred);
        failwith ("Inconsistent " ^ string_of_stmt !(!stmt))
      | _ -> assert false
    )
  | None -> false

let check_ssa_graph ?(dump = false) _graph ssa_graph =
  if dump then print_endline "Checking SSA graph";

  let open Basic_block in

  let contains block stmt =
    if not (List.mem stmt block.stmts) then (
        Printf.eprintf "Basic block %s does not contain %s\n"
          block.name (string_of_stmt !stmt);
      failwith "Inconsistent SSA graph"
    ) else true
  in

  Ssa.Graph.iter (fun x ((src, def), uses) ->
      assert (contains !src !def);
      List.iter (fun ((dst, use), _) ->
          if not (Vars.mem x (variables_in_use !(!use))) then (
            Printf.eprintf "[%s] %s -use-> [%s] %s\n"
              !src.name (string_of_stmt !(!def))
              !dst.name (string_of_stmt !(!use));
            Printf.eprintf "Variable %s is not in %s\n"
              (name_of_var x)
              (string_of_stmt !(!use));
            failwith "Inconsistent SSA graph"
          );
          assert (contains !dst !use)
        ) uses
    ) ssa_graph;
  false

(* Sequence two analyses/optimizations *)
let ( *> )
  (opt1 : ?dump:bool -> Cfg.t ref -> Ssa.Graph.t -> bool)
  (opt2 : ?dump:bool -> Cfg.t ref -> Ssa.Graph.t -> bool)
  ?(dump = false) graph ssa_graph =
  let o = opt1 graph ssa_graph ~dump in
  opt2 graph ssa_graph ~dump || o

let check = check_ssa_graph *> check_phi_functions

let optimize ?(dump = false) graph ssa_graph =
  let graph = ref graph in
  let changed = ref true in
  while !changed do
    changed := List.exists (( = ) true) [
        (simplify_control_flow *> check *> eliminate_unreachable_code *> check) graph ssa_graph ~dump;
        (eliminate_dead_code *> check) graph ssa_graph ~dump;
        (propagate_constants *> check) graph ssa_graph ~dump;
        (propagate_copies *> check) graph ssa_graph ~dump;
      ]
  done;
  !graph
