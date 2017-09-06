open Basic
open Utils

module IntSet = Set.Make (struct
  type t = int
  let compare = Pervasives.compare
end)

type cfg = node array

and node =
  { block : basic_block;
    mutable succ : IntSet.t;
    mutable pred : IntSet.t; }

let define_cfg ~(nodes : int list) ~(edges : (int * int) list) : cfg =
  let graph =
    nodes
    |> Array.of_list
    |> Array.map (fun n ->
      { block = basic_block ("B" ^ string_of_int n);
        succ = IntSet.empty;
        pred = IntSet.empty; })
  in
  (* Implicit entry and exit nodes *)
  let entry = 0 in
  let exit = Array.length graph + 1 in
  List.iter (fun (a, b) ->
      (* Add b as a successor of a, except when a = entry *)
      if a <> entry then begin
        let succ = graph.(a - 1).succ in
        graph.(a - 1).succ <- IntSet.add b succ
      end;
      (* Add a as a predecessor of b, except when b = exit *)
      if b <> exit then begin
        let pred = graph.(b - 1).pred in
        graph.(b - 1).pred <- IntSet.add a pred
      end
    ) ((entry, 1) :: edges);
  graph

let construct_cfg (basic_blocks : basic_block list) : cfg =
  let graph =
    basic_blocks
    |> Array.of_list
    |> Array.mapi (fun i n ->
        { block = n;
          succ = IntSet.empty;
          pred = IntSet.empty; })
  in
  let labels =
    basic_blocks
    |> List.mapi (fun i (Basic_block (_, source_info)) ->
        match source_info with
        | Some { entry; _ } ->
          (entry, i)
        | None ->
          invalid_arg "Basic block lacks source information")
  in
  (* Implicit entry and exit nodes *)
  let entry = 0 in
  let exit = Array.length graph + 1 in
  (* Add an edge from entry to the first basic block *)
  let pred = graph.(0).pred in
  graph.(0).pred <- IntSet.add entry pred;
  (* Connect basic blocks *)
  Array.iteri (fun i { block = Basic_block (_, source_info); succ; pred } ->
      match source_info with
      | Some { exits; _ } ->
        List.iter (function
            | "fall-through" ->
              (* Add an edge from this basic block to the next one
               * This basic block's number is i + 1, so the next one's number
               * is i + 2. *)
              let succ = graph.(i).succ in
              graph.(i).succ <- IntSet.add (i + 2) succ;
              let pred = graph.(i + 1).pred in
              graph.(i + 1).pred <- IntSet.add (i + 1) pred
            | "exit" ->
              (* Add an edge from this basic block to exit
               * Exit is not stored as a separate basic block *)
              let succ = graph.(i).succ in
              graph.(i).succ <- IntSet.add exit succ
            | label ->
              (* Add an edge from this basic block to the one starting with label *)
              let target = List.assoc label labels in
              let succ = graph.(i).succ in
              graph.(i).succ <- IntSet.add (target + 1) succ;
              let pred = graph.(target).pred in
              graph.(target).pred <- IntSet.add (i + 1) pred
          ) exits
      | None ->
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

let inspect ?dom_sets ?back_edges (graph : cfg) =
  let entry = 0 in
  let exit = Array.length graph + 1 in
  let names nodes =
    nodes
    |> IntSet.elements
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
  (* Print nodes *)
  let open Printf in
  Array.iteri (fun i { block = Basic_block (name, _); succ; pred } ->
      printf "%3s:\n\t%-12s = [%s]\n\t%-12s = [%s]\n\t%-12s = [%s]\n" name
        "successors"   (names succ)
        "predecessors" (names pred)
        "dominators"   dom_info.(i)
    ) graph;
  (* Print back edges, if known *)
  match back_edges with
  | Some edges ->
    edges
    |> List.map (fun (a, b) -> sprintf "B%d => B%d" a b)
    |> String.concat ", "
    |> printf "\nBack edges: [%s]\n"
  | None -> ()
