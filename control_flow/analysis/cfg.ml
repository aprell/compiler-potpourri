module rec Node : sig
  type t = {
    block : Basic_block.t;
    mutable pred : NodeSet.t;
    mutable succ : NodeSet.t;
    mutable doms : NodeSet.t;
    mutable idom : Node.t option;
  }
  val create : Basic_block.t -> t
  val equal : t -> t -> bool
  val ( => ) : t -> t -> unit
  val ( =|> ) : t -> t -> unit
end = struct
  type t = {
    block : Basic_block.t;
    mutable pred : NodeSet.t;
    mutable succ : NodeSet.t;
    mutable doms : NodeSet.t;
    mutable idom : Node.t option;
  }

  let create (block : Basic_block.t) : t =
    { block; pred = NodeSet.empty; succ = NodeSet.empty;
      doms = NodeSet.empty; idom = None; }

  let equal (n : t) (m : t) : bool =
    Basic_block.compare n.block m.block = 0 &&
    NodeSet.equal n.pred m.pred &&
    NodeSet.equal n.succ m.succ &&
    NodeSet.equal n.doms m.doms &&
    m.idom = n.idom

  (* Add an edge from node a to node b *)
  let ( => ) (a : t) (b : t) =
    a.block.succ <- List.sort Basic_block.compare (b.block :: a.block.succ);
    b.block.pred <- List.sort Basic_block.compare (a.block :: b.block.pred);
    a.succ <- NodeSet.add b a.succ;
    b.pred <- NodeSet.add a b.pred

  (* Remove the edge between node a and node b *)
  let ( =|> ) (a : t) (b : t) =
    a.block.succ <- List.filter (fun succ -> Basic_block.compare succ b.block <> 0) a.block.succ;
    b.block.pred <- List.filter (fun pred -> Basic_block.compare pred a.block <> 0) b.block.pred;
    a.succ <- NodeSet.remove b a.succ;
    b.pred <- NodeSet.remove a b.pred
end

and NodeSet : Set.S with type elt = Node.t = Set.Make (struct
  type t = Node.t
  let compare x y = Basic_block.compare x.Node.block y.Node.block
end)

module M = Utils.M

type t = Node.t M.t

let get_node (number : int) (graph : t) : Node.t =
  M.find number graph

let get_node_opt (number : int) (graph : t) : Node.t option =
  M.find_opt number graph

let get_entry_node (graph : t) : Node.t =
  let number, entry = M.min_binding graph in
  assert (number = 0 && entry.block.name = "Entry");
  entry

let get_exit_node (graph : t) : Node.t =
  let _, exit = M.max_binding graph in
  assert (exit.block.name = "Exit");
  exit

let get_nodes (graph : t) : Node.t list =
  M.bindings graph
  |> List.map snd

let get_basic_blocks (graph : t) : Basic_block.t list =
  get_nodes graph
  |> List.filter_map (fun { Node.block; _ } ->
      match block.name with
      | "Entry" | "Exit" -> None
      | _ -> Some block
    )

let print_basic_blocks (graph : t) =
  let open Basic_block in
  get_basic_blocks graph
  |> print_basic_blocks

let get_order (graph : t) : int =
  M.cardinal graph

let equal (a : t) (b : t) : bool =
  M.equal Node.equal a b

let iter (f : Node.t -> unit) (graph : t) =
  M.iter (fun _ node -> f node) graph

let define ~(nodes : int list) ~(edges : (int * int) list) : t =
  let open Basic_block in
  let open Node in
  let graph =
    Basic_block.(
      create 0 ~name:"Entry"
      :: List.map create nodes
      @ [create (List.length nodes + 1) ~name:"Exit"]
    )
    |> List.fold_left (fun graph block ->
        M.add block.number (Node.create block) graph
      ) M.empty
  in
  let entry = 0 in
  List.iter (fun (a, b) ->
      get_node a graph => get_node b graph
    ) ((entry, 1) :: edges);
  graph

(* Topologically sorts the nodes of a DAG *)
let dfs_reverse_postorder (graph : t) =
  let open Node in
  let visited = Hashtbl.create 10 in
  let order = ref [] in
  let exit = get_exit_node graph in
  let rec visit node =
    Hashtbl.add visited node true;
    if Basic_block.compare node.block exit.block <> 0 then (
      NodeSet.iter (fun s ->
          if not (Hashtbl.mem visited s) then visit s
        ) node.succ
    );
    order := node :: !order;
  in
  visit (get_entry_node graph);
  !order

let dfs_postorder (graph : t) =
  dfs_reverse_postorder graph
  |> List.rev

let remove_unreachable_nodes (graph : t) : t =
  let reachable_nodes = NodeSet.of_list (dfs_reverse_postorder graph) in
  let reachable node = NodeSet.mem node reachable_nodes in
  M.filter (fun _ node ->
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

let simplify (graph : t) : t =
  let open Node in

  let remove_branch node ~label =
    let target = NodeSet.find_first (fun succ ->
        Basic_block.entry_label succ.block = label
      ) node.succ
    in
    node =|> target
  in

  let simplify_branch node =
    match Basic_block.last_stmt node.block with
    | Some stmt -> (
        match !stmt with
        | Cond (Const 0, then_, else_) ->
          stmt := Jump else_;
          remove_branch node ~label:then_
        | Cond (Const 1, then_, else_) ->
          stmt := Jump then_;
          remove_branch node ~label:else_
        | _ -> ()
      )
    | None -> ()
  in

  let is_empty { Basic_block.stmts; _ } =
    List.length stmts = 2 &&
    match !(List.hd stmts), !(List.(hd (tl stmts))) with
    | Label (_, None), Jump (_, None) -> true
    | _ -> false
  in

  let retarget_branch node ~label succ =
    let label' = Basic_block.entry_label succ.block in
    begin match Basic_block.last_stmt node.block with
      | Some stmt -> (
          match !stmt with
          | Jump l when l = label ->
            stmt := Jump label';
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
    node => succ
  in

  let skip node =
    assert (NodeSet.cardinal node.succ = 1);
    let succ = NodeSet.choose node.succ in
    node =|> succ;
    NodeSet.iter (fun pred ->
        retarget_branch pred succ
          ~label:(Basic_block.entry_label node.block)
      ) node.pred;
  in

  iter (fun node ->
      simplify_branch node;
      if is_empty node.block then skip node
    ) graph;

  remove_unreachable_nodes graph

let construct (basic_blocks : Basic_block.t list) : t =
  let open Basic_block in
  let open Node in
  let graph =
    Basic_block.(
      create 0 ~name:"Entry"
      :: basic_blocks
      @ [create (List.length basic_blocks + 1) ~name:"Exit"]
    )
    |> List.fold_left (fun graph block ->
        M.add block.number (Node.create block) graph
      ) M.empty
  in
  (* Create a list that associates labels with basic block numbers *)
  let labels =
    List.map (fun block ->
        (Basic_block.entry_label block, block.number)
      ) basic_blocks
  in
  let entry = get_entry_node graph in
  let exit = get_exit_node graph in
  let node = Fun.flip get_node graph in
  (* Add an edge from entry to the first basic block *)
  entry => node 1;
  (* Connect basic blocks *)
  iter (fun { block = { name; number; _ } as block; _ } ->
      match Basic_block.last_stmt block with
      | Some { contents = Jump l } ->
        let target = List.assoc l labels in
        node number => node target
      | Some { contents = Cond (_, l1, l2) } ->
        let targets = List.(assoc l1 labels, assoc l2 labels) in
        node number => node (fst targets);
        node number => node (snd targets)
      | Some { contents = Return _ } ->
        node number => exit
      | _ ->
        assert (name = "Entry" || name = "Exit")
    ) graph;
  simplify graph

let output_dot ?filename (graph : t) =
  let open Node in
  let chan = match filename with
    | Some filename -> open_out filename
    | None -> stdout
  in
  let print ?(indent="") str =
    output_string chan (indent ^ str ^ "\n")
  in
  let indent = String.make 4 ' ' in
  print "digraph CFG {";
  iter (fun { block; succ; _ } ->
      let x = block.name in
      NodeSet.iter (fun { block; _ } ->
          let y = block.name in
          print ~indent (x ^ " -> " ^ y ^ ";")
        ) succ
    ) graph;
  print "}";
  if chan <> stdout then close_out chan

let inspect (graph : t) =
  let open Node in
  let node_name { block; _ } = block.name in
  let node_names nodes =
    nodes
    |> NodeSet.elements
    |> List.map node_name
    |> String.concat ", "
  in
  (* Print nodes *)
  let open Printf in
  iter (fun { block; pred; succ; doms; idom; } ->
      printf
        "%5s:\n \t%-12s = [%s]\n\t%-12s = [%s]\n\t%-12s = [%s]\n\t%-12s = %s\n"
        block.name
        "predecessors" (node_names pred)
        "successors"   (node_names succ)
        "dominators"   (node_names doms)
        "immediate dominator" (Option.fold idom ~some:node_name ~none:"None")
    ) graph
