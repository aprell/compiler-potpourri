open Three_address_code__IR
open Control_flow

module DU = Ssa__Def_use_chain

let remove_def x =
  let (block, def) = Option.get (DU.get_def x) in
  !block.stmts <- List.filter (( <> ) !def) !block.stmts;
  DU.remove_def x

let propagate_const x n =
  let uses = DU.get_uses x in
  DU.Set.iter (fun (_, stmt) ->
      if not (is_phi !(!stmt)) then
        !stmt := replace_stmt x (Const n) !(!stmt)
    ) uses;
  DU.filter_uses is_phi x;
  if DU.(get_uses x = Set.empty) then
    remove_def x

let propagate_copy x y =
  let uses = DU.get_uses x in
  DU.Set.iter (fun (block, stmt) ->
      if not (is_phi !(!stmt)) then (
        !stmt := replace_stmt x (Val y) !(!stmt);
        DU.add_use !block !stmt y
      )
    ) uses;
  DU.filter_uses is_phi x;
  if DU.(get_uses x = Set.empty) then
    remove_def x

let propagate' = function
  | Move (x, Const n) -> propagate_const x n
  | Move (x, Val y) -> propagate_copy x y
  | _ -> invalid_arg "propagate"

let can_optimize =
  DU.Set.exists (
    fun (_, use) ->
      not (is_phi !(!use))
  )

let propagate ?(dump = false) () =
  match (
    (* Find a constant or copy to propagate *)
    DU.find_first (fun { def; uses; } ->
        match def with
        | Some (_, stmt) -> (
            match !(!stmt) with
            | Move (_, Const _)
            | Move (_, Val _)
              when can_optimize uses -> true
            | _ -> false
          )
        | None -> false
      )
  ) with
  | Some (_, { def = Some (_, stmt); _ }) ->
    propagate' !(!stmt);
    if dump then (
      print_endline ("After propagating " ^ (string_of_stmt !(!stmt)) ^ ":");
      DU.print ();
      print_newline ()
    );
    true
  | _ -> false

let eliminate_dead_code ?(dump = false) () =
  match (
    (* Find a dead variable *)
    DU.find_first (fun { uses; _ } ->
        DU.Set.is_empty uses
      )
  ) with
  | Some (x, { def = Some (_, stmt); _ }) -> (
      match !(!stmt) with
      | Move (x, _)
      | Load (x, _)
      | Receive x ->
        (* Assume no side effects *)
        remove_def x;
        if dump then (
          print_endline ("After removing " ^ (string_of_stmt !(!stmt)) ^ ":");
          DU.print ();
          print_newline ()
        );
        true
      | Label (l, Some xs) -> (
          assert (List.mem x xs);
          !stmt := Label (l, Some (List.filter (( <> ) x) xs));
          DU.remove_def x;
          true
        )
      | _ -> assert false
    )
  | Some (_, { def = None; _ }) -> assert false
  | None -> false

let reachable { Basic_block.number; _ } graph =
  Option.is_some (Cfg.get_node_opt number graph)

let eliminate_unreachable_code ?(dump = false) graph =
  graph := Cfg.simplify !graph;
  match (
    (* Find a definition that has become unreachable *)
    DU.find_first (fun { def; _ } ->
        match def with
        | Some (block, _)
          when not (reachable !block !graph) -> true
        | _ -> false
      )
  ) with
  | Some (x, { def = Some (_, stmt); uses; }) ->
    DU.Set.iter (fun (block, stmt) ->
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
    DU.remove_def x;
    if dump then (
      print_endline ("After removing " ^ (string_of_stmt !(!stmt)) ^ ":");
      DU.print ();
      print_newline ()
    );
    true
  | _ -> false

let optimize ?(dump = false) graph =
  let graph = ref graph in
  let changed = ref true in
  while !changed do
    changed := List.exists (( = ) true) [
        eliminate_unreachable_code graph ~dump;
        eliminate_dead_code ~dump ();
        propagate ~dump ();
      ]
  done;
  !graph
