open Basic_block__Utils
open Control_flow

type 'a analysis = {
  gen : 'a;
  kill : 'a;
  mutable global_in : 'a;
  mutable global_out : 'a;
}

module type SetType = sig
  include Set.S
  val to_string : t -> string
end

(* Input signature of functors Forward_flow and Backward_flow *)
module type AnalysisType = sig
  module Set : SetType
  val meet : Set.t -> Set.t -> Set.t
  val init : Cfg.Node.t -> Cfg.t -> Set.t analysis
end

(* Output signature of functors Forward_flow and Backward_flow
   Input signature of functor Data_flow_analysis *)
module type DataFlowType = sig
  include AnalysisType
  val traverse : Cfg.t -> Cfg.Node.t list
  (* Update in-set and out-set of a node *)
  val update : Cfg.Node.t -> (string, Set.t analysis) Hashtbl.t -> Set.t * Set.t
end

module Forward_flow (A : AnalysisType) = struct
  include A

  let traverse = Cfg.dfs_reverse_postorder

  let update (node : Cfg.Node.t) sets =
    let find { Cfg.Node.block; _ } = Hashtbl.find sets block.name in
    let { gen; kill; global_in; _ } = find node in
    match node.block.name with
    | "Entry" -> (global_in, Set.union gen global_in)
    | _ -> (
        assert (Cfg.NodeSet.cardinal node.pred > 0);
        let in_set = Cfg.NodeSet.fold (fun p set ->
            A.meet (find p).global_out set
          ) node.pred (find (Cfg.NodeSet.choose node.pred)).global_out
        in
        let out_set = Set.(union gen (diff in_set kill)) in
        (in_set, out_set)
      )
end

module Backward_flow (A : AnalysisType) = struct
  include A

  let traverse = Cfg.dfs_postorder

  let update (node : Cfg.Node.t) sets =
    let find { Cfg.Node.block; _ } = Hashtbl.find sets block.name in
    let { gen; kill; global_out; _ } = find node in
    match node.block.name with
    | "Exit" -> (Set.union gen global_out, global_out)
    | _ -> (
        assert (Cfg.NodeSet.cardinal node.succ > 0);
        let out_set = Cfg.NodeSet.fold (fun s set ->
            A.meet (find s).global_in set
          ) node.succ (find (Cfg.NodeSet.choose node.succ)).global_in
        in
        let in_set = Set.(union gen (diff out_set kill)) in
        (in_set, out_set)
      )
end

module Data_flow_analysis (DF : DataFlowType) = struct
  let compare_rows xs ys =
    match xs, ys with
    | "Entry" :: _, _ | _, "Exit" :: _ -> -1
    | "Exit" :: _, _ | _, "Entry" :: _ -> 1
    | _, _ -> Stdlib.compare xs ys

  let print_gen_kill sets =
    let rows =
      Hashtbl.fold (fun name { gen; kill; _ } rows ->
          [ name;
            DF.Set.to_string gen;
            DF.Set.to_string kill ] :: rows
        ) sets []
      |> List.sort compare_rows
    in
    print_table ~rows:([""; "gen"; "kill"] :: rows)

  let print_in_out sets =
    let rows =
      Hashtbl.fold (fun name { global_in; global_out; _ } rows ->
          [ name;
            DF.Set.to_string global_in;
            DF.Set.to_string global_out ] :: rows
        ) sets []
      |> List.sort compare_rows
    in
    print_table ~rows:([""; "IN"; "OUT"] :: rows)

  let init graph =
    List.fold_left (fun tbl ({ Cfg.Node.block; _ } as node) ->
        Hashtbl.add tbl block.name (DF.init node graph); tbl
      ) (Hashtbl.create 10) (Cfg.get_nodes graph)

  let compute ?(dump = false) graph =
    let traversal = DF.traverse graph in
    let sets = init graph in
    let find { Cfg.Node.block; _ } = Hashtbl.find sets block.name in
    let changed = ref true in
    let num_iter = ref 1 in
    if dump then (
      print_endline "Local sets:";
      print_gen_kill sets;
      print_newline ();
      print_endline "Initialization:";
      print_in_out sets;
      print_newline ()
    );
    while !changed do
      changed := false;
      List.iter (fun node ->
          let in_set', out_set' = DF.update node sets in
          if (not (DF.Set.equal in_set' (find node).global_in) ||
              not (DF.Set.equal out_set' (find node).global_out)) then
            changed := true;
          (find node).global_in <- in_set';
          (find node).global_out <- out_set'
        ) traversal;
      if dump then (
        print_endline ("Iteration " ^ (string_of_int !num_iter) ^ ":");
        print_in_out sets;
        print_newline ()
      );
      incr num_iter
    done;
    sets
end
