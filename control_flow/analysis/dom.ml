open Cfg

let dominators (graph : Cfg.t) : NodeSet.t array =
  let open Node in
  let entry = 0 in
  (* Initialization *)
  Array.iter (fun node ->
      if node.index = entry || unreachable node then
        node.doms <- NodeSet.singleton node
      else
        node.doms <- NodeSet.of_list (Array.to_list graph)
    ) graph;
  (* Iteration *)
  let changed = ref true in
  let num_iter = ref 1 in
  while !changed do
    changed := false;
    Array.iter (fun node ->
        let doms' =
          node.doms
          (* Intersect the dominators of all predecessors of B *)
          |> NodeSet.fold (fun p set -> NodeSet.inter p.doms set) node.pred
          (* Union resulting set with {B} *)
          |> NodeSet.union (NodeSet.singleton node)
        in
        if not (NodeSet.equal doms' node.doms) then changed := true;
        node.doms <- doms'
      ) graph;
    incr num_iter
  done;
  Array.map (fun { doms; _ } -> doms) graph

let immediate_dominators (graph : Cfg.t) : Node.t option array =
  let open Node in
  let entry = 0 in
  let rec immediate_dominator node =
    if node.index = entry || unreachable node then
      NodeSet.empty
    else
      let idom = ref (NodeSet.inter node.doms node.pred) in
      if NodeSet.is_empty !idom then (
        (* The immediate dominator is not a direct predecessor *)
        try NodeSet.iter (fun p ->
            let idom' = NodeSet.inter (immediate_dominator p) node.doms in
            if not (NodeSet.is_empty idom') then (
              idom := idom';
              raise Exit
            )
          ) node.pred
        with Exit -> ()
      );
      assert (NodeSet.cardinal !idom = 1);
      !idom
  in
  Array.iter (fun node ->
      node.idom <- (
        match NodeSet.elements (immediate_dominator node) with
        | [idom] -> Some idom
        | [] -> None
        | _ -> assert false
      )
    ) graph;
  Array.map (fun { idom; _ } -> idom) graph

(* Find all edges i => n with n dom i in a graph *)
let back_edges (graph : Cfg.t) : (Node.t * Node.t) list =
  let open Node in
  let dom n i = NodeSet.mem n i.doms in
  let back_edges = ref [] in
  Array.iter (fun i ->
      NodeSet.iter (fun n ->
          if dom n i then
            back_edges := (i, n) :: !back_edges
        ) i.succ
    ) graph;
  List.rev !back_edges

module Domtree = struct
  type t = Cfg.t
  type elt = Node.t

  (* Add an edge from node a to node b *)
  let ( => ) (a : elt) (b : elt) =
    a.succ <- NodeSet.add b a.succ;
    b.pred <- NodeSet.add a b.pred

  (* Create a graph containing every node of the CFG, and for every node n, add
   * an edge n.idom => n. Since n has at most one immediate dominator, this
   * graph is a tree, the dominator tree. *)
  let create (graph : Cfg.t) : t =
    let open Node in
    let tree = Array.map (fun node ->
        { node with succ = NodeSet.empty; pred = NodeSet.empty; }) graph
    in
    Array.iter (fun node ->
        match node.idom with
        | Some idom -> tree.(idom.index) => tree.(node.index)
        | None -> ()
      ) tree;
    tree

  let children (node : elt) : elt list =
    NodeSet.elements node.succ

  let output_dot ?filename (tree : t) =
    let open Node in
    let chan = match filename with
      | Some filename -> open_out filename
      | None -> stdout
    in
    let print ?(indent="") str =
      output_string chan (indent ^ str ^ "\n")
    in
    let indent = String.make 4 ' ' in
    print "graph DominatorTree {";
    Array.iter (fun { block = Basic_block (x, _); succ; _; } ->
        NodeSet.iter (fun node ->
            let Basic_block (y, _) = node.block in
            print ~indent (x ^ " -- " ^ y ^ ";")
          ) succ
      ) tree;
    print "}";
    if chan <> stdout then close_out chan

  let inspect tree = Cfg.inspect tree
end
