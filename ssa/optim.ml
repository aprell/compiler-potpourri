open Three_address_code__IR

let rec replace_expr x y = function
  | Val x' when x' = x -> y
  | Binop (op, e1, e2) ->
    constant_fold (Binop (op, replace_expr x y e1, replace_expr x y e2))
  | Relop (op, e1, e2) ->
    constant_fold (Relop (op, replace_expr x y e1, replace_expr x y e2))
  | e -> e

let replace_list x = function
  | Val y -> List.map (fun z -> if z = x then y else z)
  | _ -> invalid_arg "replace_list"

let replace_stmt x y = function
  | Move (x', e) ->
    Move (x', replace_expr x y e)
  | Load (x', Deref x'') as load -> (
      match y with
      | Val y when x = x'' -> Load (x', Deref y)
      | _ -> load
    )
  | Store (Deref x', e) -> (
      let e' = replace_expr x y e in
      match y with
      | Val y when x = x' -> Store (Deref y, e')
      | _ -> Store (Deref x', e')
    )
  | Jump (l, Some xs) ->
    Jump (l, Some (replace_list x y xs))
  | Cond (e, (l1, None), (l2, None)) ->
    Cond (replace_expr x y e, (l1, None), (l2, None))
  | Cond (e, (l1, Some xs), (l2, Some ys)) ->
    Cond (replace_expr x y e, (l1, Some (replace_list x y xs)), (l2, Some (replace_list x y ys)))
  | Return (Some e) ->
    Return (Some (replace_expr x y e))
  | Phi (x', xs) ->
    Phi (x', replace_list x y xs)
  | s -> s

let propagate_phi x y =
  let uses = Def_use_chain.get_uses x in
  Def_use_chain.Set.iter (fun (block, stmt) ->
      !stmt := replace_stmt x (Val y) !(!stmt);
      Def_use_chain.add_use !block !stmt y
    ) uses;
  Def_use_chain.remove_uses x;
  Def_use_chain.remove_def x

let remove_def x =
  let (block, def) = Option.get (Def_use_chain.get_def x) in
  let src = Option.get !block.source in
  src.stmts <- List.filter (( <> ) !def) src.stmts;
  Def_use_chain.remove_def x

let propagate_const x n =
  let uses = Def_use_chain.get_uses x in
  Def_use_chain.Set.iter (fun (_, stmt) ->
      if not (is_phi !(!stmt)) then
        !stmt := replace_stmt x (Const n) !(!stmt)
    ) uses;
  Def_use_chain.filter_uses is_phi x;
  if Def_use_chain.(get_uses x = Set.empty) then
    remove_def x

let propagate_copy x y =
  let uses = Def_use_chain.get_uses x in
  Def_use_chain.Set.iter (fun (block, stmt) ->
      if not (is_phi !(!stmt)) then (
        !stmt := replace_stmt x (Val y) !(!stmt);
        Def_use_chain.add_use !block !stmt y
      )
    ) uses;
  Def_use_chain.filter_uses is_phi x;
  if Def_use_chain.(get_uses x = Set.empty) then
    remove_def x

let propagate' = function
  | Move (x, Const n) -> propagate_const x n
  | Move (x, Val y) -> propagate_copy x y
  | Phi (x, [y]) -> propagate_phi x y
  | _ -> invalid_arg "propagate"

let can_optimize =
  Def_use_chain.Set.exists (
    fun (_, use) ->
      not (is_phi !(!use))
  )

let propagate ?(dump = false) () =
  match (
    (* Find a constant or copy to propagate *)
    Def_use_chain.find_first (fun { def; uses; } ->
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
      Def_use_chain.print ();
      print_newline ()
    );
    true
  | _ -> false

let eliminate_dead_code ?(dump = false) () =
  match (
    (* Find a dead variable *)
    Def_use_chain.find_first (fun { uses; _ } ->
        Def_use_chain.Set.is_empty uses
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
          Def_use_chain.print ();
          print_newline ()
        );
        true
      | Label (l, Some xs) -> (
          assert (List.mem x xs);
          !stmt := Label (l, Some (List.filter (( <> ) x) xs));
          Def_use_chain.remove_def x;
          true
        )
      | _ -> assert false
    )
  | Some (x, { def = None; _ }) ->
    (* Remove leftover binding from SSA conversion *)
    Def_use_chain.remove_def x;
    true
  | None -> false

let optimize ?(dump = false) () =
  let changed = ref true in
  while !changed do
    changed := List.exists (( = ) true) [
        eliminate_dead_code ~dump ();
        propagate ~dump ();
      ]
  done
