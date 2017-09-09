open Cfg

let dominators (graph : cfg) : IntSet.t array =
  let num_basic_blocks = Array.length graph in
  let entry = 0 in
  let exit = num_basic_blocks - 1 in
  (* Initialization *)
  let doms = Array.init num_basic_blocks (fun i ->
      if i = entry then
        IntSet.singleton entry
      else
        IntSet.of_list Utils.(entry -- exit))
  in
  (* Iteration *)
  let changed = ref true in
  let num_iter = ref 1 in
  while !changed do
    changed := false;
    Array.iteri (fun i dom ->
        let pred = graph.(i).pred in
        let dom' =
          dom
          (* Intersect the dominators of all predecessors of B *)
          |> IntSet.fold (fun p set -> IntSet.inter doms.(p) set) pred
          (* Union resulting set with {B} *)
          |> IntSet.union (IntSet.singleton i)
        in
        if dom' <> dom then changed := true;
        doms.(i) <- dom'
      ) doms;
    incr num_iter
  done;
  doms

(* Find all edges i => n with n dom i in a graph *)
let back_edges (graph : cfg) (dom_sets : IntSet.t array) : (int * int) list =
  (* dom_sets.(i) are the dominators of node graph.(i) *)
  let dom n i = IntSet.mem n dom_sets.(i) in
  let back_edges = ref [] in
  Array.iter (fun { index = i; succ; _ } ->
      IntSet.iter (fun n ->
          if dom n i then
            back_edges := (i, n) :: !back_edges
        ) succ
    ) graph;
  List.rev !back_edges
