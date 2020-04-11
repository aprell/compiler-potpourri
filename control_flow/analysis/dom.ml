open Cfg
open Utils

let ( -- ) = Basic__Utils.( -- )

let dominators (graph : cfg) : Nodes.t array =
  let num_basic_blocks = Array.length graph in
  let entry = 0 in
  let exit = num_basic_blocks - 1 in
  (* Initialization *)
  Array.iter (fun node ->
      if node.index = entry || unreachable node then
        node.doms <- Nodes.singleton node.index
      else
        node.doms <- Nodes.of_list (entry -- exit)
    ) graph;
  (* Iteration *)
  let changed = ref true in
  let num_iter = ref 1 in
  while !changed do
    changed := false;
    Array.iteri (fun i node ->
        let doms' =
          node.doms
          (* Intersect the dominators of all predecessors of B *)
          |> Nodes.fold (fun p set -> Nodes.inter graph.(p).doms set) node.pred
          (* Union resulting set with {B} *)
          |> Nodes.union (Nodes.singleton i)
        in
        if not (Nodes.equal doms' node.doms) then changed := true;
        node.doms <- doms'
      ) graph;
    incr num_iter
  done;
  Array.map (fun { doms; _ } -> doms) graph

let immediate_dominators (graph : cfg) : Nodes.elt option array =
  let entry = 0 in
  let rec immediate_dominator node =
    if node.index = entry || unreachable node then
      Nodes.empty
    else
      let idom = ref (Nodes.inter node.doms node.pred) in
      if Nodes.is_empty !idom then (
        (* The immediate dominator is not a direct predecessor *)
        try Nodes.iter (fun p ->
            let idom' = Nodes.inter (immediate_dominator graph.(p)) node.doms in
            if not (Nodes.is_empty idom') then (
              idom := idom';
              raise Exit
            )
          ) node.pred
        with Exit -> ()
      );
      assert (Nodes.cardinal !idom = 1);
      !idom
  in
  Array.iter (fun node ->
      node.idom <- (
        match Nodes.elements (immediate_dominator node) with
        | [idom] -> Some idom
        | [] -> None
        | _ -> assert false
      )
    ) graph;
  Array.map (fun { idom; _ } -> idom) graph

(* Find all edges i => n with n dom i in a graph *)
let back_edges (graph : cfg) : (Nodes.elt * Nodes.elt) list =
  (* graph.(i).doms are the dominators of node with index i *)
  let dom n i = Nodes.mem n graph.(i).doms in
  let back_edges = ref [] in
  Array.iter (fun { index = i; succ; _ } ->
      Nodes.iter (fun n ->
          if dom n i then
            back_edges := (i, n) :: !back_edges
        ) succ
    ) graph;
  List.rev !back_edges
