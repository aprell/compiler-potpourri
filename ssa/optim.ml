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

(* Eliminate moves with constant propagation (1) and copy propagation (2):
 * (1) x := c => replace Val (Var x) with Const c
 * (2) x := y => replace Val (Var x) with Val (Var y) *)
let propagate move =
  let x, y = match move with
    | Move (x, y) -> x, y
    | _ -> invalid_arg "propagate"
  in
  let uses = Def_use_chain.get_uses x in
  Def_use_chain.Set.iter (fun (block, use) ->
      !use := replace_stmt x y !(!use);
      match y with
      | Val y -> Def_use_chain.add_use !block !use y
      | _ -> ()
    ) uses;
  Def_use_chain.remove_uses x;
  let (block, def) = Option.get (Def_use_chain.get_def x) in
  let src = Option.get !block.source in
  src.stmts <- List.filter (( <> ) !def) src.stmts;
  Def_use_chain.remove_def x

let optimize ?(dump = false) () =
  let changed = ref true in
  let num_iter = ref 1 in
  while !changed do
    changed := false;
    Def_use_chain.iter (fun x def _ ->
        match def with
        | Some (_, stmt) -> (
            match !(!stmt) with
            | Move (_, Const _)
            | Move (_, Val (Var _)) ->
              propagate !(!stmt);
              assert (Def_use_chain.(get_uses x = Set.empty));
              if dump then (
                print_endline ("After iteration " ^ (string_of_int !num_iter) ^ ":");
                Def_use_chain.print ();
                print_newline ()
              );
              changed := true;
              incr num_iter
            | _ -> ()
          )
        | None -> ()
      )
  done
