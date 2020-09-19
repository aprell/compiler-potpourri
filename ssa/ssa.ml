open Three_address_code__IR
open Basic_block
open Control_flow
open Utils

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
                  match !(List.hd (List.rev stmts)) with
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

  Array.iter (fun node ->
      if node.block.name <> "Entry" && node.block.name <> "Exit" then
        let pred = List.map (fun node -> node.block) (elements node.pred) in
        (* Insert phi-functions for every labeled jump (not only at join
         * points, where two or more control flow paths merge) *)
        if List.length pred > 0 then
          node.block <- insert node.block ~pred
    ) graph

let minimize_phi_functions graph =
  let open Cfg in
  let open Cfg.Node in

  let worklist = Queue.create () in
  let pending = Queue.create () in

  let copy_propagate move stmts =
    let x, y = match move with
      | Move (x, y) -> x, y
      | _ -> invalid_arg "copy_propagate"
    in
    let rec loop acc = function
      | [Jump (_, Some xs) as jump]
      | [Cond (_, (_, Some xs), _) as jump] ->
        if List.mem x xs then Queue.add move pending;
        Optim.replace_stmt x y jump :: acc
      | [Return _ as ret] ->
        Optim.replace_stmt x y ret :: acc
      | stmt :: stmts ->
        loop (Optim.replace_stmt x y stmt :: acc) stmts
      | [] ->
        (* Every (reachable) basic block ends with a jump or return *)
        assert false
    in
    List.rev (loop [] stmts)
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
          (* Replace x := PHI(x, x') or x := PHI(x', x') with x := x' and
           * perform copy propagation *)
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
        let stmts = List.map ( ! ) stmts in
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
        Basic_block.update block ~stmts:(List.map ref stmts)
      )
    | None ->
      assert (block.name = "Entry" || block.name = "Exit");
      block
  in

  (* Seed work list *)
  Array.iter (fun node ->
      Queue.add (fun () ->
          node.block <- remove_phi_functions node
        ) worklist
    ) graph;

  (* Iterate *)
  while not (Queue.is_empty worklist) do
    Queue.take worklist ()
  done;

  let remove_label_params = function
    | Jump (l, Some _) -> Jump (l, None)
    | Cond (e, (l1, Some _), (l2, Some _)) -> Cond (e, (l1, None), (l2, None))
    | s -> s
  in

  (* Remove remaining label parameters *)
  Array.iter (fun ({ block; _ } as node) ->
      node.block <- match block.source with
        | Some { stmts; _ } ->
          Basic_block.update node.block ~stmts:
            (List.map (( ! ) >> remove_label_params >> ref) stmts)
        | None ->
          assert (block.name = "Entry" || block.name = "Exit");
          block
    ) graph

(* Roughly follows the simple generation of SSA form by Aycock and Horspool:
 * (1) Insert phi-functions "everywhere" (the "really crude" approach)
 * (2) Delete unnecessary phi-functions, optimize, and repeat
 * The result is minimal, or close to minimal, SSA form *)
let convert_to_ssa graph =
  parameterize_labels graph;
  rename_variables graph;
  insert_phi_functions graph;
  minimize_phi_functions graph
