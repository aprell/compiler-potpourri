open Cfg

type back_edge = Node.t * Node.t

type t = {
  head : Node.t;
  tail : Node.t;
  nodes : NodeSet.t;
}

(* Find all edges i => n with n dom i in a graph *)
let back_edges (graph : Cfg.t) : back_edge list =
  let open Node in
  let dom n i = NodeSet.mem n i.doms in
  let back_edges = ref [] in
  Cfg.iter (fun i ->
      NodeSet.iter (fun n ->
          if dom n i then
            back_edges := (i, n) :: !back_edges
        ) i.succ
    ) graph;
  List.rev !back_edges

let find ((tail, head) : back_edge) : t =
  let open Node in
  let nodes = ref (NodeSet.of_list [head]) in
  let rec visit node =
    if not (node.block.name = head.block.name) then
      if not (NodeSet.mem node !nodes) then (
        nodes := NodeSet.add node !nodes;
        NodeSet.iter visit node.pred
      )
  in
  visit tail;
  { head; tail; nodes = !nodes }

let node_name { Node.block; _ } = block.name

let print (loop : t) =
  Printf.printf "%s -> %s: {%s}\n"
    (node_name loop.tail)
    (node_name loop.head)
    (String.concat ", " (List.map node_name (NodeSet.elements loop.nodes)))

module NestingForest = struct
  module M = Utils.M

  type t = elt M.t

  and elt = {
    block : Basic_block.t;
    mutable children : elt list;
  }

  let compare a b = Basic_block.compare a.block b.block

  (* Connect parent a and child b *)
  let ( -- ) a b =
    a.children <- List.sort compare (b :: a.children)

  let create (graph : Cfg.t) : t =
    let open Node in
    let loops =
      back_edges graph
      |> List.map find
      |> List.sort (fun loop1 loop2 ->
          NodeSet.(cardinal loop1.nodes - cardinal loop2.nodes))
    in
    let forest =
      get_nodes graph
      |> List.fold_left (fun forest { block; _ } ->
          M.add block.number { block; children = [] } forest
        ) M.empty
    in
    let parents = Hashtbl.create 10 in
    let has_parent = Hashtbl.mem parents in
    let add_parent = Hashtbl.add parents in
    List.iter (fun loop ->
        let x = loop.head.block.number in
        NodeSet.iter (fun node ->
            let y = node.block.number in
            if x <> y && not (has_parent y) then (
              M.find x forest -- M.find y forest;
              add_parent y x
            )
          ) loop.nodes
      ) loops;
    forest

  let output_dot ?filename (forest : t) =
    let chan = match filename with
      | Some filename -> open_out filename
      | None -> stdout
    in
    let print ?(indent="") str =
      output_string chan (indent ^ str ^ "\n")
    in
    let indent = String.make 4 ' ' in
    print "graph LoopNestingForest {";
    M.iter (fun _ { block; children; } ->
        let x = block.name in
        List.iter (fun { block; _ } ->
            let y = block.name in
            print ~indent (x ^ " -- " ^ y ^ ";")
          ) children
      ) forest;
    print "}";
    if chan <> stdout then close_out chan
end
