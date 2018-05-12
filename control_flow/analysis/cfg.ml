open Basic
open Utils

type cfg = node array

and node =
  { index : int;
    block : basic_block;
    mutable succ : IntSet.t;
    mutable pred : IntSet.t; }

(* Add an edge from node a to node b *)
let ( => ) a b =
  a.succ <- IntSet.add b.index a.succ;
  b.pred <- IntSet.add a.index b.pred

let define_cfg ~(nodes : int list) ~(edges : (int * int) list) : cfg =
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
        { index; block; succ = IntSet.empty; pred = IntSet.empty; })
  in
  let entry = 0 in
  List.iter (fun (a, b) ->
      graph.(a) => graph.(b)
    ) ((entry, 1) :: edges);
  graph

let construct_cfg (basic_blocks : basic_block list) : cfg =
  let graph =
    basic_block "Entry" :: basic_blocks @ [basic_block "Exit"]
    |> Array.of_list
    |> Array.mapi (fun index block ->
        { index; block; succ = IntSet.empty; pred = IntSet.empty; })
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
  Array.iteri (fun i { block = Basic_block (_, source_info); succ; pred; _ } ->
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
  graph

let discard_source_info (graph : cfg) : cfg =
  Array.map (fun ({ block = Basic_block (name, _); _ } as node) ->
      { node with block = basic_block name }) graph

let equal (a : cfg) (b : cfg) : bool =
  if Array.length a <> Array.length b then false
  else
    let ab = Array.map2 (fun node_a node_b -> (node_a, node_b)) a b in
    not (Array.exists (fun (node_a, node_b) ->
        node_a.block <> node_b.block ||
        not (IntSet.equal node_a.succ node_b.succ) ||
        not (IntSet.equal node_a.pred node_b.pred)) ab)
