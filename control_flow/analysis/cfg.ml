module rec Node : sig
  type t = {
    block : Basic_block.t;
    mutable pred : NodeSet.t;
    mutable succ : NodeSet.t;
    mutable doms : NodeSet.t;
    mutable idom : Node.t option;
  }
end = Node
(* https://blog.janestreet.com/a-trick-recursive-modules-from-recursive-signatures *)

and NodeSet : Set.S with type elt = Node.t = Set.Make (struct
  type t = Node.t
  let compare x y = Basic_block.compare x.Node.block y.Node.block
end)

let create_node (block : Basic_block.t) : Node.t =
  { block; succ = NodeSet.empty; pred = NodeSet.empty;
    doms = NodeSet.empty; idom = None; }

module M = Utils.M

type t = Node.t M.t

let get_node (number : int) (graph : t) : Node.t =
  M.find number graph

let get_entry_node (graph : t) : Node.t =
  let number, entry = M.min_binding graph in
  assert (number = 0);
  entry

let get_exit_node (graph : t) : Node.t =
  let _, exit = M.max_binding graph in
  exit

let get_nodes (graph : t) : Node.t list =
  M.bindings graph
  |> List.map snd

let basic_blocks (graph : t) : Basic_block.t list =
  get_nodes graph
  |> List.map (fun { Node.block; _ } -> block)

let print_basic_blocks (graph : t) =
  let open Basic_block in
  basic_blocks graph
  |> List.filter (fun { name; _ } -> name <> "Entry" && name <> "Exit")
  |> print_basic_blocks

let get_order (graph : t) : int =
  M.cardinal graph

(* Add an edge from node a to node b *)
let ( => ) (a : Node.t) (b : Node.t) =
  a.block.succ <- List.sort Basic_block.compare (b.block :: a.block.succ);
  b.block.pred <- List.sort Basic_block.compare (a.block :: b.block.pred);
  a.succ <- NodeSet.add b a.succ;
  b.pred <- NodeSet.add a b.pred

let define ~(nodes : int list) ~(edges : (int * int) list) : t =
  let open Basic_block in
  let graph =
    Basic_block.(
      create 0 ~name:"Entry"
      :: List.map create nodes
      @ [create (List.length nodes + 1) ~name:"Exit"]
    )
    |> List.fold_left (fun graph block ->
        M.add block.number (create_node block) graph
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

let prune_unreachable_nodes (graph : t) : t =
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

let iter (f : Node.t -> unit) (graph : t) =
  M.iter (fun _ node -> f node) graph

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
        M.add block.number (create_node block) graph
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
  prune_unreachable_nodes graph

let equal (a : t) (b : t) : bool =
  let open Node in
  let cmp n m =
    Basic_block.compare n.block m.block = 0 &&
    NodeSet.equal n.pred m.pred &&
    NodeSet.equal n.succ m.succ &&
    NodeSet.equal n.doms m.doms &&
    m.idom = n.idom
  in
  M.equal cmp a b

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
