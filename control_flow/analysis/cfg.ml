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

and NodeSet : Set.S with type elt = Node.t = Set.Make(struct
  type t = Node.t
  let compare x y = Basic_block.compare x.Node.block y.Node.block
end)

type t = Node.t array

(* Add an edge from node a to node b *)
let ( => ) (a : Node.t) (b : Node.t) =
  a.block.succ <- List.sort Basic_block.compare (b.block :: a.block.succ);
  b.block.pred <- List.sort Basic_block.compare (a.block :: b.block.pred);
  a.succ <- NodeSet.add b a.succ;
  b.pred <- NodeSet.add a b.pred

let define ~(nodes : int list) ~(edges : (int * int) list) : t =
  let open Node in
  let graph =
    Basic_block.(
      create 0 ~name:"Entry"
      :: List.map create nodes
      @ [create (List.length nodes + 1) ~name:"Exit"]
    )
    |> Array.of_list
    |> Array.map (fun block ->
        { block; succ = NodeSet.empty; pred = NodeSet.empty;
          doms = NodeSet.empty; idom = None; })
  in
  let entry = 0 in
  List.iter (fun (a, b) ->
      graph.(a) => graph.(b)
    ) ((entry, 1) :: edges);
  graph

(* Topologically sorts the nodes of a DAG *)
let dfs_reverse_postorder (graph : t) =
  let open Node in
  let num_nodes = Array.length graph in
  let visited = Array.make num_nodes false in
  let order = ref [] in
  let entry = 0 in
  let exit = num_nodes - 1 in
  let rec visit node =
    visited.(node.block.number) <- true;
    if node.block.number <> exit then (
      NodeSet.iter (fun s ->
          if not visited.(s.block.number) then visit s
        ) node.succ
    );
    order := node :: !order;
  in
  visit graph.(entry);
  !order

let dfs_postorder (graph : t) =
  dfs_reverse_postorder graph
  |> List.rev

let prune_unreachable_nodes (graph : t) : t =
  let reachable_nodes = NodeSet.of_list (dfs_reverse_postorder graph) in
  let reachable node = NodeSet.mem node reachable_nodes in
  if NodeSet.cardinal reachable_nodes < Array.length graph then
    Array.iter (fun node ->
        if not (reachable node) then (
          node.block.stmts <- [];
          NodeSet.iter (fun succ ->
              if (reachable succ) then
                succ.block.pred <- List.filter (fun pred ->
                    Basic_block.compare pred node.block <> 0
                  ) succ.block.pred;
            ) node.succ;
          node.succ <- NodeSet.empty;
          node.pred <- NodeSet.empty
        ) else (
          assert (NodeSet.for_all reachable node.succ);
          node.pred <- NodeSet.filter reachable node.pred
        )
      ) graph;
  graph

let unreachable (node : Node.t) =
  node.succ = NodeSet.empty && node.pred = NodeSet.empty

let iter (f : Node.t -> unit) (graph : t) =
  Array.iter (fun node ->
      if unreachable node then ()
      else f node
    ) graph

let construct (basic_blocks : Basic_block.t list) : t =
  let open Node in
  let graph =
    Basic_block.(
      create 0 ~name:"Entry"
      :: basic_blocks
      @ [create (List.length basic_blocks + 1) ~name:"Exit"]
    )
    |> Array.of_list
    |> Array.map (fun block ->
        { block; succ = NodeSet.empty; pred = NodeSet.empty;
          doms = NodeSet.empty; idom = None; })
  in
  (* Create a list that associates labels with basic block numbers *)
  let labels =
    List.map (fun block ->
        (Basic_block.entry_label block, block.number)
      ) basic_blocks
  in
  let entry = 0 in
  let exit = Array.length graph - 1 in
  (* Add an edge from entry to the first basic block *)
  graph.(entry) => graph.(1);
  (* Connect basic blocks *)
  Array.iteri (fun i { block; _ } ->
      match Basic_block.last_stmt block with
      | Some { contents = Jump l } ->
        let target = List.assoc l labels in
        graph.(i) => graph.(target)
      | Some { contents = Cond (_, l1, l2) } ->
        let targets = List.(assoc l1 labels, assoc l2 labels) in
        graph.(i) => graph.(fst targets);
        graph.(i) => graph.(snd targets)
      | Some { contents = Return _ } ->
        graph.(i) => graph.(exit)
      | _ ->
        assert (i = entry || i = exit)
    ) graph;
  prune_unreachable_nodes graph

let basic_blocks (graph : t) : Basic_block.t list =
  let open Node in
  Array.fold_left (fun blocks ({ block; _ } as node) ->
      if unreachable node then blocks
      else block :: blocks
    ) [] graph
  |> List.rev

let print_basic_blocks (graph : t) =
  let open Basic_block in
  basic_blocks graph
  |> List.filter (fun { name; _ } -> name <> "Entry" && name <> "Exit")
  |> print_basic_blocks

let equal (a : t) (b : t) : bool =
  let open Node in
  if Array.length a <> Array.length b then false
  else
    let ab = Array.map2 (fun node_a node_b -> (node_a, node_b)) a b in
    not (Array.exists (fun (node_a, node_b) ->
        Basic_block.compare node_a.block node_b.block <> 0 ||
        not (NodeSet.equal node_a.succ node_b.succ) ||
        not (NodeSet.equal node_a.pred node_b.pred) ||
        not (NodeSet.equal node_a.doms node_b.doms) ||
        node_a.idom <> node_b.idom) ab)

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
  Array.iter (fun { block; succ; _ } ->
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
  Array.iter (fun { block; succ; pred; doms; idom; _ } ->
      printf
        "%5s:\n \t%-12s = [%s]\n\t%-12s = [%s]\n\t%-12s = [%s]\n\t%-12s = %s\n"
        block.name
        "successors"   (node_names succ)
        "predecessors" (node_names pred)
        "dominators"   (node_names doms)
        "immediate dominator" (Option.fold idom ~some:node_name ~none:"None")
    ) graph
