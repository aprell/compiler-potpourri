open Control_flow__Cfg
open Control_flow__Utils

type 'a analysis =
  { gen : 'a;
    kill : 'a;
    mutable global_in : 'a;
    mutable global_out : 'a; }

(* Input signatures of functors Forward_flow and Backward_flow *)
module type SetType = sig
  include Set.S
end

module type AnalysisType = sig
  type t
  val meet : t -> t -> t
  val init : node -> cfg -> t analysis
end

(* Output signature of functors Forward_flow and Backward_flow
   Input signature of functor Data_flow_analysis *)
module type DataFlowType = sig
  type t
  val traverse : cfg -> Nodes.elt list
  val init : cfg -> t analysis array
  (* Update in-set and out-set of a node *)
  val update : node -> t analysis array -> t * t
end

module Forward_flow (S : SetType) (T : AnalysisType with type t := S.t) = struct
  type t = S.t

  let traverse = dfs_reverse_postorder

  let init graph =
    let num_nodes = Array.length graph in
    Array.init num_nodes (fun i -> T.init graph.(i) graph)

  let update (n : node) sets =
    let { index = i; block = Basic_block (name, _); pred; _ } = n in
    let { gen; kill; global_in; _ } = sets.(i) in
    match name with
    | "Entry" -> (global_in, S.union gen global_in)
    | _ -> (
        assert (Nodes.cardinal pred > 0);
        let in_set = Nodes.fold (fun p set ->
            T.meet sets.(p).global_out set
          ) pred (sets.(Nodes.choose pred).global_out)
        in
        let out_set = S.union gen (S.diff in_set kill) in
        (in_set, out_set)
      )
end

module Backward_flow (S : SetType) (T : AnalysisType with type t := S.t) = struct
  type t = S.t

  let traverse = dfs_postorder

  let init graph =
    let num_nodes = Array.length graph in
    Array.init num_nodes (fun i -> T.init graph.(i) graph)

  let update (n : node) sets =
    let { index = i; block = Basic_block (name, _); succ; _ } = n in
    let { gen; kill; global_out; _ } = sets.(i) in
    match name with
    | "Exit" -> (S.union gen global_out, global_out)
    | _ -> (
        assert (Nodes.cardinal succ > 0);
        let out_set = Nodes.fold (fun s set ->
            T.meet sets.(s).global_in set
          ) succ (sets.(Nodes.choose succ).global_in)
        in
        let in_set = S.union gen (S.diff out_set kill) in
        (in_set, out_set)
      )
end

module Data_flow_analysis (T : DataFlowType) = struct
  let compute graph =
    let traversal = T.traverse graph in
    let sets = T.init graph in
    let changed = ref true in
    let num_iter = ref 1 in
    while !changed do
      changed := false;
      List.iter (fun i ->
          let in_set', out_set' = T.update graph.(i) sets in
          if (in_set' <> sets.(i).global_in || out_set' <> sets.(i).global_out) then
            changed := true;
          sets.(i).global_in <- in_set';
          sets.(i).global_out <- out_set'
        ) traversal;
      incr num_iter
    done;
    sets
end
