open Three_address_code__IR
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

let optimize ?(dump = false) graph ssa_graph =
  let graph = ref graph in
  let changed = ref true in
  while !changed do
    changed := List.exists (( = ) true) [
        eliminate_unreachable_code graph ssa_graph ~dump;
        eliminate_dead_code ssa_graph ~dump;
        propagate_constants graph ssa_graph ~dump;
        propagate_copies ssa_graph ~dump;
      ]
  done;
  !graph
