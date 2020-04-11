open Basic
open Cfg
open Utils

let inspect (graph : cfg) =
  let entry = 0 in
  let exit = Array.length graph - 1 in
  let names nodes =
    nodes
    |> Nodes.elements
    |> List.map (fun n ->
        if n = entry then "Entry"
        else if n = exit then "Exit"
        else "B" ^ string_of_int n)
    |> String.concat ", "
  in
  (* Print nodes *)
  let open Printf in
  Array.iter (fun { block = Basic_block (name, _); succ; pred; doms; idom; _ } ->
      printf
        "%5s:\n \t%-12s = [%s]\n\t%-12s = [%s]\n\t%-12s = [%s]\n\t%-12s = %s\n"
        name
        "successors"   (names succ)
        "predecessors" (names pred)
        "dominators"   (names doms)
        "immediate dominator" (names idom)
    ) graph;
  (* Print back edges *)
  Dom.back_edges graph
  |> List.map (fun (a, b) -> sprintf "B%d => B%d" a b)
  |> String.concat ", "
  |> printf "\nBack edges: [%s]\n"
