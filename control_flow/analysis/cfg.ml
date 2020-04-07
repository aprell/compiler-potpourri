open Basic
open Utils

type cfg = node array

and node =
  { index : Nodes.elt;
    block : basic_block;
    mutable succ : Nodes.t;
    mutable pred : Nodes.t; }

(* Add an edge from node a to node b *)
let ( => ) a b =
  a.succ <- Nodes.add b.index a.succ;
  b.pred <- Nodes.add a.index b.pred

let define_cfg ~(nodes : Nodes.elt list) ~(edges : (Nodes.elt * Nodes.elt) list) : cfg =
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
        { index; block; succ = Nodes.empty; pred = Nodes.empty; })
  in
  let entry = 0 in
  List.iter (fun (a, b) ->
      graph.(a) => graph.(b)
    ) ((entry, 1) :: edges);
  graph

(* Topologically sorts the nodes of a DAG *)
let dfs_reverse_postorder (graph : cfg) =
  let num_nodes = Array.length graph in
  let visited = Array.make num_nodes false in
  let order = ref [] in
  let entry = 0 in
  let exit = num_nodes - 1 in
  let rec visit node =
    visited.(node) <- true;
    if node <> exit then (
      let succ = graph.(node).succ in
      Nodes.iter (fun s -> if not visited.(s) then visit s) succ
    );
    order := node :: !order;
  in
  visit entry;
  !order

let dfs_postorder (graph : cfg) =
  dfs_reverse_postorder graph
  |> List.rev

let prune_unreachable_nodes (graph : cfg) : cfg =
  let reachable_nodes = Nodes.of_list (dfs_reverse_postorder graph) in
  let reachable node = Nodes.mem node reachable_nodes in
  if Nodes.cardinal reachable_nodes < Array.length graph then
    Array.iteri (fun i node ->
        if not (reachable i) then (
          node.succ <- Nodes.empty;
          node.pred <- Nodes.empty
        ) else (
          assert (Nodes.for_all reachable node.succ);
          node.pred <- Nodes.filter reachable node.pred
        )
      ) graph;
  graph

let unreachable node =
  node.succ = Nodes.empty && node.pred = Nodes.empty

let construct_cfg (basic_blocks : basic_block list) : cfg =
  let graph =
    basic_block "Entry" :: basic_blocks @ [basic_block "Exit"]
    |> Array.of_list
    |> Array.mapi (fun index block ->
        { index; block; succ = Nodes.empty; pred = Nodes.empty; })
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

let discard_source_info (graph : cfg) : cfg =
  Array.map (fun ({ block = Basic_block (name, _); _ } as node) ->
      { node with block = basic_block name }) graph

let equal (a : cfg) (b : cfg) : bool =
  if Array.length a <> Array.length b then false
  else
    let ab = Array.map2 (fun node_a node_b -> (node_a, node_b)) a b in
    not (Array.exists (fun (node_a, node_b) ->
        node_a.block <> node_b.block ||
        not (Nodes.equal node_a.succ node_b.succ) ||
        not (Nodes.equal node_a.pred node_b.pred)) ab)

let output_dot ?filename (graph : cfg) =
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
      Nodes.iter (fun node ->
          let Basic_block (y, _) = graph.(node).block in
          print ~indent (x ^ " -> " ^ y ^ ";")
        ) succ
    ) graph;
  print "}";
  if chan <> stdout then close_out chan
