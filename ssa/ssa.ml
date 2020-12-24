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

  let parameterize { block; _ } =
    let rec loop = function
      | stmt :: stmts -> (
          match !stmt with
          | Label (l, None) ->
            stmt := Label (l, Some variables)
          | Jump (l, None) ->
            stmt := Jump (l, Some variables)
          | Cond (e, (l1, None), (l2, None)) ->
            stmt := Cond (e, (l1, Some variables), (l2, Some variables))
          | _ -> ()
        );
        loop stmts
      | [] -> ()
    in
    match block.source with
    | Some { stmts; _ } ->
      loop stmts
    | None ->
      assert (block.name = "Entry" || block.name = "Exit")
  in

  Cfg.iter parameterize graph

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
    | Val x -> Val (rename_variable x ~bump:false)
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
    let open Value_numbering in
    match !stmt with
    | Move (x, e) ->
      (* Evaluation order matters *)
      let e' = rename_variables_expr e in
      let x' = rename_variable x ~bump:true in
      let vn = value_number e' in
      let _ = match e with
        | Binop _ | Relop _ when Hashtbl.mem available_exprs vn ->
          let y = Hashtbl.find available_exprs vn in
          stmt := Move (x', Val y)
        | _ ->
          stmt := Move (x', e');
          Hashtbl.add available_exprs vn x'
      in
      Hashtbl.add value_numbers (name_of_var x') vn
    | Load (x, Deref y) ->
      let y' = rename_variable y ~bump:false in
      let x' = rename_variable x ~bump:true in
      stmt := Load (x', Deref y')
    | Store (Deref x, e) ->
      let e' = rename_variables_expr e in
      let x' = rename_variable x ~bump:false in
      stmt := Store (Deref x', e')
    | Label (l, Some xs) ->
      let xs' = List.map (rename_variable ~bump:true) xs in
      stmt := Label (l, Some xs')
    | Label (_, None) -> ()
    | Jump (l, Some xs) ->
      let xs' = List.map (rename_variable ~bump:false) xs in
      stmt := Jump (l, Some xs')
    | Jump (_, None) -> ()
    | Cond (e, (l1, Some xs), (l2, Some ys)) -> (
      let e' = rename_variables_expr e in
      let xs' = List.map (rename_variable ~bump:false) xs in
      let ys' = List.map (rename_variable ~bump:false) ys in
      let vn = value_number e' in
      match Hashtbl.find_opt available_exprs vn with
      | Some x -> stmt := Cond (Val x, (l1, Some xs'), (l2, Some ys'))
      | None -> stmt := Cond (e', (l1, Some xs'), (l2, Some ys'))
    )
    | Cond (e, then_, else_) -> (
      let e' = rename_variables_expr e in
      let vn = value_number e' in
      match Hashtbl.find_opt available_exprs vn with
      | Some x -> stmt := Cond (Val x, then_, else_)
      | None -> stmt := Cond (e', then_, else_)
    )
    | Receive x ->
      let x' = rename_variable x ~bump:true in
      stmt := Receive x'
    | Return (Some e) -> (
      let e' = rename_variables_expr e in
      let vn = value_number e' in
      match Hashtbl.find_opt available_exprs vn with
      | Some x -> stmt := Return (Some (Val x))
      | None -> stmt := Return (Some e')
    )
    | Return None -> ()
    | _ -> assert false
  in

  let rename { block; _ } =
    match block.source with
    | Some { stmts; _ } ->
      List.iter rename_variables_stmt stmts
    | None ->
      assert (block.name = "Entry" || block.name = "Exit")
  in

  Cfg.iter rename graph

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
    | Some ({ stmts; _ } as src) -> (
        assert (stmts <> []);
        match !(List.hd stmts) with
        | Label (l, _) as label ->
          let jumps = List.fold_left (fun jumps block ->
              match block.source with
              | Some { stmts; _ } -> (
                  match !(last stmts) with
                  | Jump _ | Cond _ as jump -> jump :: jumps
                  | _ -> assert false
                )
              | None -> jumps
            ) [] pred |> List.rev
          in
          if jumps = [] then (
            assert (block.name = "B1");
            assert (List.length pred = 1);
            assert ((List.hd pred).name = "Entry")
          ) else (
            let phi_funcs = create_phi_functions label jumps in
            (* Insert phi-functions and erase label parameters *)
            src.stmts <- List.map ref (Label (l, None) :: phi_funcs) @ List.tl stmts
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
        if List.length pred > 0 then insert node.block ~pred
    ) graph;

  (* Erase remaining label parameters and build def-use chains *)
  Cfg.iter (fun { block; _ } ->
      erase_label_params block;
      Def_use_chain.build block
    ) graph

let minimize_phi_functions graph =
  let open Cfg.Node in

  let worklist = Queue.create () in

  let rec add_task block =
    Queue.add (fun () ->
        remove_phi_functions block
      ) worklist

  and remove_phi_functions block =
    let has_def =
      Def_use_chain.get_def >> Option.is_some
    in
    let rec loop = function
      | stmt :: stmts -> (
          match !stmt with
          | Phi (x, [x']) -> (
              (* Replace x := PHI(x') with x := x' and perform copy propagation *)
              propagate x x';
              loop stmts
            )
          | Phi (x, xs) -> (
              let xs = S.of_list (List.filter has_def xs) in
              if S.cardinal xs = 1 && not (S.mem x xs) ||
                 S.cardinal xs = 2 && S.mem x xs then (
                (* Replace x := PHI(x, x') or x := PHI(x', x') with x := x' and
                 * perform copy propagation *)
                let x' = S.find_first (( <> ) x) xs in
                propagate x x';
                loop stmts
              ) else (
                (* Keep this phi-function *)
                stmt :: loop stmts
              )
            )
          | _ -> stmt :: loop stmts
        )
      | stmts -> stmts
    in
    match block.source with
    | Some ({ stmts; _ } as src) ->
      src.stmts <- loop stmts
    | None ->
      assert (block.name = "Entry" || block.name = "Exit")

  and propagate x y =
    Def_use_chain.basic_blocks_of_uses x
    |> List.iter (( ! ) >> add_task);
    Optim.propagate_phi x y
  in

  (* Seed work list *)
  Cfg.iter (fun { block; _ } ->
      add_task block
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
