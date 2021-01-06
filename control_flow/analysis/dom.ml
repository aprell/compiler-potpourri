open Cfg

let dominators (graph : Cfg.t) : NodeSet.t array =
  let open Node in
  let entry = 0 in
  (* Initialization *)
  Array.iter (fun node ->
      if node.block.number = entry || unreachable node then
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
  let idoms = Array.make (Array.length graph) None in
  let rec immediate_dominator node =
    if node.block.number = entry || unreachable node then
      NodeSet.empty
    else
      let idom = ref (NodeSet.inter node.doms node.pred) in
      if NodeSet.is_empty !idom then (
        (* The immediate dominator is not a direct predecessor *)
        try NodeSet.iter (fun p ->
            let idom' = NodeSet.inter (
                match idoms.(p.block.number) with
                | Some idom -> NodeSet.singleton idom
                | None -> immediate_dominator p
              ) node.doms
            in
            if not (NodeSet.is_empty idom') then (
              idom := idom';
              raise Exit
            )
          ) node.pred
        with Exit -> ()
      );
      assert (NodeSet.cardinal !idom = 1);
      idoms.(node.block.number) <- Some (NodeSet.choose !idom);
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
  idoms

module Domtree = struct
  type t = Cfg.t
  type elt = Node.t

  (* Add an edge between node a and node b *)
  let ( -- ) (a : elt) (b : elt) =
    a.succ <- NodeSet.add b a.succ;
    b.pred <- NodeSet.add a b.pred

  (* Create a graph containing every node of the CFG, and for every node n, add
   * an edge n.idom -- n. Since n has at most one immediate dominator, this
   * graph is a tree, the dominator tree. *)
  let create (graph : Cfg.t) : t =
    let open Node in
    let tree = Array.map (fun node ->
        { node with succ = NodeSet.empty;
                    pred = NodeSet.empty; }) graph
    in
    Array.iter (fun node ->
        match node.idom with
        | Some idom -> tree.(idom.block.number) -- tree.(node.block.number)
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
    Cfg.iter (fun { block; succ; _ } ->
        let x = block.name in
        NodeSet.iter (fun { block; _ } ->
            let y = block.name in
            print ~indent (x ^ " -- " ^ y ^ ";")
          ) succ
      ) tree;
    print "}";
    if chan <> stdout then close_out chan

  let inspect = Cfg.inspect
end

(* The dominance frontier of a node n is the set of nodes where n's dominance
 * stops. Formally, the dominance frontier of node n includes all nodes d such
 * that n dom p, p in pred(d), and not n sdom d, where sdom denotes strict
 * dominance, i.e., n sdom d if n dom d and n != d. *)
let dominance_frontiers (graph : Cfg.t) (domtree : Domtree.t) : NodeSet.t array =
  let open Node in
  let idom { block = a; _ } { idom; _ } =
    match idom with
    | Some { block = b; _ } -> a.number = b.number
    | None -> false
  in
  let dom n i = NodeSet.mem n i.doms in
  let df = Array.make (Array.length graph) NodeSet.empty in
  let rec dominance_frontier ({ block; succ; _ } as node) =
    NodeSet.iter (fun s ->
        if not (idom node s) then (
          assert (not (dom node s));
          df.(block.number) <- NodeSet.union df.(block.number) (NodeSet.singleton s)
        )
      ) succ;
    List.iter (fun { block = child; _ } ->
        NodeSet.iter (fun n ->
            if not (dom node n) then
              df.(block.number) <- NodeSet.union df.(block.number) (NodeSet.singleton n)
          ) (dominance_frontier graph.(child.number))
      ) (Domtree.children domtree.(block.number));
    df.(block.number)
  in
  let _ = dominance_frontier graph.(0) in df
