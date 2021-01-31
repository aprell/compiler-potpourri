open Three_address_code__IR
open Basic_block
open Control_flow
open Ssa__Utils

module S = Liveness.Set

let parameterize_labels graph =
  let open Cfg.Node in

  (* Collect all variables that are live on entry to _some_ basic block
   * (see semi-pruned SSA form) *)
  let variables = List.fold_left (fun non_locals { block; _ } ->
      let use, _ = Liveness.compute block in
      S.union use non_locals
    ) S.empty (Cfg.get_nodes graph)
  |> S.elements
  in

  let parameterize { block; _ } =
    let visit stmt =
      match !stmt with
      | Label (l, None) ->
        stmt := Label (l, Some variables)
      | Jump (l, None) ->
        stmt := Jump (l, Some variables)
      | Cond (e, (l1, None), (l2, None)) ->
        stmt := Cond (e, (l1, Some variables), (l2, Some variables))
      | _ -> ()
    in
    List.iter visit block.stmts
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
    List.iter rename_variables_stmt block.stmts
  in

  Cfg.iter rename graph

let insert_phi_functions graph =
  let rec create_phi_functions res args =
    match res with
    | x :: xs ->
      Phi (x, List.(map hd) args)
      :: create_phi_functions xs (List.(map tl) args)
    | [] -> []
  in

  let insert block =
    match first_stmt block with
    | Some { contents = Label (l, Some xs) } ->
      let args =
        block.pred
        |> List.fold_left (fun args block ->
            match last_stmt block with
            | Some { contents = Jump (l', Some ys) } ->
              assert (l' = l);
              ys :: args
            | Some { contents = Cond (_, (l1, Some ys), _) } when l1 = l ->
              ys :: args
            | Some { contents = Cond (_, _, (l2, Some ys)) } when l2 = l ->
              ys :: args
            | Some _ -> assert false
            | None -> args
          ) []
        |> List.rev
      in
      if args = [] then (
        assert (block.name = "B1");
        assert (List.length block.pred = 1);
        assert ((List.hd block.pred).name = "Entry")
      ) else (
        let phi_funcs = create_phi_functions xs args in
        (* Insert phi-functions and erase label parameters *)
        block.stmts <- List.map ref (Label (l, None) :: phi_funcs) @ List.tl block.stmts
      )
    | _ -> assert false
  in

  let erase_label_params block =
    match last_stmt block with
    | Some branch -> (
        match !branch with
        | Jump (l, Some _) ->
          branch := Jump (l, None)
        | Cond (e, (l1, Some _), (l2, Some _)) ->
          branch := Cond (e, (l1, None), (l2, None))
        | _ -> ()
      )
    | None ->
      assert (block.name = "Entry" || block.name = "Exit")
  in

  Cfg.iter (fun { block; _ } ->
      if block.name <> "Entry" && block.name <> "Exit" then
        (* Insert phi-functions for every labeled jump (not only at join
         * points, where two or more control flow paths merge) *)
        insert block
    ) graph;

  (* Erase remaining label parameters *)
  Cfg.iter (fun { block; _ } ->
      erase_label_params block
    ) graph

let minimize_phi_functions graph =
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
    block.stmts <- loop block.stmts

  and propagate x y =
    Def_use_chain.get_uses x
    |> Def_use_chain.Set.elements
    |> List.map (fst >> ( ! ))
    |> List.sort_uniq Basic_block.compare
    |> List.iter add_task;
    propagate_phi x y

  and propagate_phi x y =
    let uses = Def_use_chain.get_uses x in
    Def_use_chain.Set.iter (fun (block, stmt) ->
        !stmt := replace_stmt x (Val y) !(!stmt);
        Def_use_chain.add_use !block !stmt y
      ) uses;
    Def_use_chain.remove_uses x;
    Def_use_chain.remove_def x
  in

  (* Build def-use chains and seed work list *)
  Cfg.iter (fun { block; _ } ->
      Def_use_chain.build block;
      add_task block
    ) graph;

  (* Iterate *)
  while not (Queue.is_empty worklist) do
    Queue.take worklist ()
  done

module Graph = struct
  type t = (var, def_use) Hashtbl.t
  and def_use = def * use_def list
  and use_def = use * def
  and def = Def_use_chain.Set.elt
  and use = Def_use_chain.Set.elt

  let create () =
    let graph = Hashtbl.create 10 in
    Def_use_chain.iter (fun x def uses ->
        match def with
        | Some def ->
          let uses =
            Def_use_chain.Set.elements uses
            |> List.map (fun use -> (use, def))
          in
          Hashtbl.add graph x (def, uses)
        | None ->
          (* Dead SSA name *)
          assert (Def_use_chain.Set.is_empty uses)
      );
    Def_use_chain.clear ();
    graph

  let get_def_use (x : var) (graph : t) =
    Hashtbl.find_opt graph x

  let get_def (x : var) (graph : t) =
    match get_def_use x graph with
    | Some (def, _) -> Some def
    | None -> None

  let get_use_def (x : var) (graph : t) =
    match get_def_use x graph with
    | Some (_, uses) -> uses
    | None -> []

  let get_uses (x : var) (graph : t) =
    get_use_def x graph
    |> List.map fst

  let add_use (x : var) (use : use) (graph : t) =
    match get_def_use x graph with
    | Some (def, uses) -> (
        try ignore (List.assoc use uses)
        with Not_found ->
          Hashtbl.replace graph x (def, (use, def) :: uses)
      )
    | None -> ()

  let set_uses (x : var) (uses : use list) (graph : t) =
    match get_def x graph with
    | Some def ->
      Hashtbl.replace graph x (def, List.map (fun use -> (use, def)) uses)
    | None -> ()

  let remove_use (x : var) ((_, stmt) : use) (graph : t) =
    let filter = List.filter (fst >> snd >> (( <> )) stmt) in
    match get_def_use x graph with
    | Some (def, uses) ->
      assert (uses <> []);
      Hashtbl.replace graph x (def, filter uses)
    | None -> ()

  let remove_uses (x : var) (graph : t) =
    match get_def x graph with
    | Some def ->
      Hashtbl.replace graph x (def, [])
    | None -> ()

  let remove_def (x : var) (graph : t) =
    match get_def x graph with
    | Some ((_, stmt) as def) -> (
        match !(!stmt) with
        | Move (_, e) ->
          List.iter (fun y -> remove_use y def graph) (all_variables_expr e)
        | Load (_, Deref y) ->
          remove_use y def graph
        | Label (_, Some ys)
        | Phi (_, ys) ->
          List.iter (fun y -> remove_use y def graph) ys
        | _ -> assert false
      );
      Hashtbl.remove graph x
    | None -> ()

  let iter = Hashtbl.iter

  let find_first p (graph : t) =
    let first = ref None in
    begin
      try iter (fun x def ->
          if p def then (
            first := Some (x, def);
            raise_notrace Exit
          )
        ) graph
      with Exit -> ()
    end;
    !first

  let to_string ((def, uses) : def_use) =
    let string_of_stmt' (block, stmt) =
      Printf.sprintf "%s (%s)" (string_of_stmt !(!stmt)) !block.name
    in
    string_of_stmt' def,
    String.concat ", " (List.map (fst >> string_of_stmt') uses)

  let print (graph : t) =
    iter (fun (Var x) def_use_chain ->
        let def, uses = to_string def_use_chain in
        Printf.printf "%s: def = %s, uses = [%s]\n" x def uses
      ) graph

  let node_name_of_stmt (block, stmt) =
    let rec loop i = function
      | s :: _ when s = !stmt -> i
      | _ :: stmts -> loop (i + 1) stmts
      | [] -> failwith "node_name_of_stmt"
    in
    Printf.sprintf "%s_%d" !block.name (loop 1 !block.stmts)

  let node_label_of_stmt (_, stmt) =
    string_of_stmt !(!stmt)

  let output_dot ?filename graph =
    let chan = match filename with
      | Some filename -> open_out filename
      | None -> stdout
    in
    let print ?(indent="") str =
      output_string chan (indent ^ str ^ "\n")
    in
    let indent = String.make 4 ' ' in
    print "digraph SSA {";
    print ~indent "node [shape=box];";
    iter (fun (Var v) (def, uses) ->
        let x = node_name_of_stmt def in
        print ~indent (x ^ " [label=\"" ^ node_label_of_stmt def ^ "\"];");
        List.iter (fun (use, _) ->
            let y = node_name_of_stmt use in
            print ~indent (y ^ " [label=\"" ^ node_label_of_stmt use ^ "\"];");
            print ~indent (x ^ " -> " ^ y ^ " [label=\"use (" ^ v ^ ")\"];");
            print ~indent (y ^ " -> " ^ x ^ " [label=\"def (" ^ v ^ ")\"];")
          ) uses
      ) graph;
    print "}";
    if chan <> stdout then close_out chan
end

(* Roughly follows the simple generation of SSA form by Aycock and Horspool:
 * (1) Insert phi-functions "everywhere" (the "really crude" approach)
 * (2) Delete unnecessary phi-functions, optimize, and repeat
 * The result is minimal, or close to minimal, SSA form *)
let convert_to_ssa graph =
  parameterize_labels graph;
  rename_variables graph;
  insert_phi_functions graph;
  minimize_phi_functions graph;
  Graph.create ()
