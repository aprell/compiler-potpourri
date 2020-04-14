open Basic

module rec Node : sig
  type t = {
    index : int;
    block : basic_block;
    mutable succ : NodeSet.t;
    mutable pred : NodeSet.t;
    mutable doms : NodeSet.t;
    mutable idom : Node.t option;
  }
end = Node
(* https://blog.janestreet.com/a-trick-recursive-modules-from-recursive-signatures *)

and NodeSet : Set.S with type elt = Node.t = Set.Make(struct
  type t = Node.t
  let compare x y = Stdlib.compare x.Node.index y.Node.index
end)

type t = Node.t array

(* Add an edge from node a to node b *)
let ( => ) (a : Node.t) (b : Node.t) =
  a.succ <- NodeSet.add b a.succ;
  b.pred <- NodeSet.add a b.pred

let define ~(nodes : int list) ~(edges : (int * int) list) : t =
  let open Node in
  let basic_blocks =
    List.map (fun i ->
        let name = "B" ^ string_of_int i in
        basic_block name
      ) nodes
  in
  let graph =
    basic_block "Entry" :: basic_blocks @ [basic_block "Exit"]
    |> Array.of_list
    |> Array.mapi (fun index block ->
        { index; block; succ = NodeSet.empty; pred = NodeSet.empty;
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
    visited.(node.index) <- true;
    if node.index <> exit then (
      NodeSet.iter (fun s -> if not visited.(s.index) then visit s) node.succ
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

let construct (basic_blocks : basic_block list) : t =
  let open Node in
  let graph =
    basic_block "Entry" :: basic_blocks @ [basic_block "Exit"]
    |> Array.of_list
    |> Array.mapi (fun index block ->
        { index; block; succ = NodeSet.empty; pred = NodeSet.empty;
          doms = NodeSet.empty; idom = None; })
  in
  (* Create a list that associates labels with basic blocks *)
  let labels =
    List.mapi (fun i (Basic_block (_, source_info)) ->
        match source_info with
        | Some { entry = label; _ } ->
          (label, i + 1)
        | None ->
          invalid_arg "Basic block lacks source information"
      ) basic_blocks
  in
  let entry = 0 in
  let exit = Array.length graph - 1 in
  (* Add an edge from entry to the first basic block *)
  graph.(entry) => graph.(1);
  (* Connect basic blocks *)
  Array.iteri (fun i { block = Basic_block (_, source_info); _ } ->
      match source_info with
      | Some { exits; _ } ->
        List.iter (function
            | "fall-through" ->
              graph.(i) => graph.(i + 1)
            | "exit" ->
              graph.(i) => graph.(exit)
            | label ->
              let target = List.assoc label labels in
              graph.(i) => graph.(target)
          ) exits
      | None ->
        if i <> entry && i <> exit then
          invalid_arg "Basic block lacks source information"
    ) graph;
  prune_unreachable_nodes graph

let discard_source_info (graph : t) : t =
  let open Node in
  Array.map (fun ({ block = Basic_block (name, _); _ } as node) ->
      { node with block = basic_block name }) graph

let equal (a : t) (b : t) : bool =
  let open Node in
  if Array.length a <> Array.length b then false
  else
    let ab = Array.map2 (fun node_a node_b -> (node_a, node_b)) a b in
    not (Array.exists (fun (node_a, node_b) ->
        node_a.block <> node_b.block ||
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
  Array.iter (fun { block = Basic_block (x, _); succ; _; } ->
      NodeSet.iter (fun node ->
          let Basic_block (y, _) = node.block in
          print ~indent (x ^ " -> " ^ y ^ ";")
        ) succ
    ) graph;
  print "}";
  if chan <> stdout then close_out chan
