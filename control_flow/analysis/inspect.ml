open Basic
open Cfg
open Utils

let inspect ?dom_sets ?idoms ?back_edges (graph : cfg) =
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
  let dom_info = match dom_sets with
    | Some doms -> Array.map names doms
    | None -> Array.make (Array.length graph) "?"
  in
  let idom_info = match idoms with
    | Some idoms -> Array.map names idoms
    | None -> Array.make (Array.length graph) "?"
  in
  (* Print nodes *)
  let open Printf in
  Array.iteri (fun i { block = Basic_block (name, _); succ; pred; _ } ->
      printf
        "%5s:\n \t%-12s = [%s]\n\t%-12s = [%s]\n\t%-12s = [%s]\n\t%-12s = %s\n"
        name
        "successors"   (names succ)
        "predecessors" (names pred)
        "dominators"   dom_info.(i)
        "immediate dominator" idom_info.(i)
    ) graph;
  (* Print back edges, if known *)
  match back_edges with
  | Some edges ->
    edges
    |> List.map (fun (a, b) -> sprintf "B%d => B%d" a b)
    |> String.concat ", "
    |> printf "\nBack edges: [%s]\n"
  | None -> ()
