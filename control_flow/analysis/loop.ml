open Cfg

type back_edge = Node.t * Node.t

type t = {
  head : Node.t;
  tail : Node.t;
  nodes : NodeSet.t;
}

(* Find all edges i => n with n dom i in a graph *)
let back_edges graph =
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

let find (tail, head) =
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

let print loop =
  let node_name { Node.block; _ } = block.name in
  Printf.printf "%s => %s: {%s}\n"
    (node_name loop.tail)
    (node_name loop.head)
    (String.concat ", " (List.map node_name (NodeSet.elements loop.nodes)))
