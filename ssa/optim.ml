open Three_address_code__IR
open Control_flow

let remove_def x ssa_graph =
  let block, stmt = Option.get (Ssa.Graph.get_def x ssa_graph) in
  !block.stmts <- List.filter (( <> ) !stmt) !block.stmts;
  Ssa.Graph.remove_def x ssa_graph

let propagate_const x n ssa_graph =
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

let propagate_copy x y ssa_graph =
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

let propagate' stmt ssa_graph =
  match stmt with
  | Move (x, Const n) -> propagate_const x n ssa_graph
  | Move (x, Val y) -> propagate_copy x y ssa_graph
  | _ -> invalid_arg "propagate'"

let can_optimize =
  List.exists (
    fun ((_, use), _) ->
      not (is_phi !(!use))
  )

let propagate ?(dump = false) ssa_graph =
  match (
    (* Find a constant or copy to propagate *)
    Ssa.Graph.find_first (fun ((_, stmt), uses) ->
        match !(!stmt) with
        | Move (_, Const _)
        | Move (_, Val _)
          when can_optimize uses -> true
        | _ -> false
      ) ssa_graph
  ) with
  | Some (_, ((_, stmt), _)) ->
    propagate' !(!stmt) ssa_graph;
    if dump then (
      print_endline ("After propagating " ^ (string_of_stmt !(!stmt)) ^ ":");
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
        | _ -> uses = []
      ) ssa_graph
  ) with
  | Some (_, ((_, stmt), _)) -> (
      match !(!stmt) with
      | Move (x, _)
      | Load (x, _)
      | Receive x ->
        (* Assume no side effects *)
        remove_def x ssa_graph;
        if dump then (
          print_endline ("After removing " ^ (string_of_stmt !(!stmt)) ^ ":");
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
  graph := Cfg.simplify !graph;
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
              | xs -> !stmt := Phi (xn, xs)
            )
          | _ -> assert false
        )
      ) uses;
    Ssa.Graph.remove_def x ssa_graph;
    if dump then (
      print_endline ("After removing " ^ (string_of_stmt !(!stmt)) ^ ":");
      Ssa.Graph.print ssa_graph;
      print_newline ()
    );
    true
  | _ -> false

let optimize ?(dump = false) graph ssa_graph =
  let graph = ref graph in
  let changed = ref true in
  while !changed do
    changed := List.exists (( = ) true) [
        eliminate_unreachable_code graph ssa_graph ~dump;
        eliminate_dead_code ssa_graph ~dump;
        propagate ssa_graph ~dump;
      ]
  done;
  !graph
