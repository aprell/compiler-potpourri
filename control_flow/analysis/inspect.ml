open Basic
open Cfg

let inspect (graph : Cfg.t) =
  let open Node in
  let entry = 0 in
  let exit = Array.length graph - 1 in
  let node_name node =
    if node.index = entry then "Entry"
    else if node.index = exit then "Exit"
    else "B" ^ string_of_int node.index
  in
  let node_names nodes =
    nodes
    |> NodeSet.elements
    |> List.map node_name
    |> String.concat ", "
  in
  (* Print nodes *)
  let open Printf in
  Array.iter (fun { block = Basic_block (name, _); succ; pred; doms; idom; _ } ->
      printf
        "%5s:\n \t%-12s = [%s]\n\t%-12s = [%s]\n\t%-12s = [%s]\n\t%-12s = %s\n"
        name
        "successors"   (node_names succ)
        "predecessors" (node_names pred)
        "dominators"   (node_names doms)
        "immediate dominator" (Option.fold ~none:"None" ~some:node_name idom)
    ) graph;
  (* Print back edges *)
  Dom.back_edges graph
  |> List.map (fun (a, b) -> sprintf "%s => %s" (node_name a) (node_name b))
  |> String.concat ", "
  |> printf "\nBack edges: [%s]\n"
