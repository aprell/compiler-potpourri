open Cfg

let dominators (graph : Cfg.t) =
  let open Node in
  let entry = 0 in
  (* Initialization *)
  Cfg.iter (fun node ->
      if node.block.number = entry then
        node.doms <- NodeSet.singleton node
      else
        node.doms <- NodeSet.of_list (get_nodes graph)
    ) graph;
  (* Iteration *)
  let changed = ref true in
  let num_iter = ref 1 in
  while !changed do
    changed := false;
    Cfg.iter (fun node ->
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
  done

let immediate_dominators (graph : Cfg.t) =
  let open Node in
  let entry = 0 in
  let rec immediate_dominator node =
    if node.block.number = entry then
      NodeSet.empty
    else
      let idom = ref (NodeSet.inter node.doms node.pred) in
      if NodeSet.is_empty !idom then (
        (* The immediate dominator is not a direct predecessor *)
        try NodeSet.iter (fun p ->
            let idom' = NodeSet.inter (
                match p.idom with
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
      !idom
  in
  Cfg.iter (fun node ->
      node.idom <- (
        match NodeSet.elements (immediate_dominator node) with
        | [idom] -> Some idom
        | [] -> None
        | _ -> assert false
      )
    ) graph

module Domtree = struct
  module M = Map.Make (struct
    type t = int
    let compare = Stdlib.compare
  end)

  type t = elt M.t

  and elt = {
    block : Basic_block.t;
    mutable children : elt list;
  }

  let get_node = M.find

  let compare a b = Basic_block.compare a.block b.block

  (* Connect parent a and child b *)
  let ( -- ) a b =
    a.children <- List.sort compare (b :: a.children)

  (* Create a graph containing every node of the CFG, and for every node n, add
   * an edge n.idom -- n. Since n has at most one immediate dominator, this
   * graph is a tree, the dominator tree. *)
  let create (graph : Cfg.t) : t =
    let open Node in
    let tree =
      get_nodes graph
      |> List.fold_left (fun tree { block; _ } ->
          M.add block.number { block; children = [] } tree
        ) M.empty
    in
    Cfg.iter (fun node ->
        match node.idom with
        | Some idom ->
          let parent = get_node idom.block.number tree in
          let child = get_node node.block.number tree in
          parent -- child
        | None -> ()
      ) graph;
    tree

  let output_dot ?filename (tree : t) =
    let chan = match filename with
      | Some filename -> open_out filename
      | None -> stdout
    in
    let print ?(indent="") str =
      output_string chan (indent ^ str ^ "\n")
    in
    let indent = String.make 4 ' ' in
    print "graph DominatorTree {";
    M.iter (fun _ { block; children; } ->
        let x = block.name in
        List.iter (fun { block; _ } ->
            let y = block.name in
            print ~indent (x ^ " -- " ^ y ^ ";")
          ) children
      ) tree;
    print "}";
    if chan <> stdout then close_out chan
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
  let df = Array.make (get_order graph) NodeSet.empty in
  let rec dominance_frontier ({ block; succ; _ } as node) =
    NodeSet.iter (fun s ->
        if not (idom node s) then (
          assert (not (dom node s));
          df.(block.number) <- NodeSet.union df.(block.number) (NodeSet.singleton s)
        )
      ) succ;
    List.iter (fun ({ block = child; _ } : Domtree.elt) ->
        NodeSet.iter (fun n ->
            if not (dom node n) then
              df.(block.number) <- NodeSet.union df.(block.number) (NodeSet.singleton n)
          ) (dominance_frontier (get_node graph child.number))
      ) (Domtree.get_node block.number domtree).children;
    df.(block.number)
  in
  let _ = dominance_frontier (get_entry_node graph) in df
