open Control_flow

type 'a analysis = {
  gen : 'a;
  kill : 'a;
  mutable global_in : 'a;
  mutable global_out : 'a;
}

(* Input signatures of functors Forward_flow and Backward_flow *)
module type SetType = sig
  include Set.S
end

module type AnalysisType = sig
  type t
  val meet : t -> t -> t
  val init : Cfg.Node.t -> Cfg.t -> t analysis
end

(* Output signature of functors Forward_flow and Backward_flow
   Input signature of functor Data_flow_analysis *)
module type DataFlowType = sig
  type t
  val traverse : Cfg.t -> Cfg.Node.t list
  val init : Cfg.t -> t analysis array
  (* Update in-set and out-set of a node *)
  val update : Cfg.Node.t -> t analysis array -> t * t
end

module Forward_flow (S : SetType) (T : AnalysisType with type t := S.t) = struct
  type t = S.t

  let traverse = Cfg.dfs_reverse_postorder

  let init graph =
    let num_nodes = Array.length graph in
    Array.init num_nodes (fun i -> T.init graph.(i) graph)

  let update (node : Cfg.Node.t) sets =
    let { gen; kill; global_in; _ } = sets.(node.index) in
    match node.block.name with
    | "Entry" -> (global_in, S.union gen global_in)
    | _ -> (
        assert (Cfg.NodeSet.cardinal node.pred > 0);
        let in_set = Cfg.NodeSet.fold (fun p set ->
            T.meet sets.(p.index).global_out set
          ) node.pred (sets.((Cfg.NodeSet.choose node.pred).index).global_out)
        in
        let out_set = S.union gen (S.diff in_set kill) in
        (in_set, out_set)
      )
end

module Backward_flow (S : SetType) (T : AnalysisType with type t := S.t) = struct
  type t = S.t

  let traverse = Cfg.dfs_postorder

  let init graph =
    let num_nodes = Array.length graph in
    Array.init num_nodes (fun i -> T.init graph.(i) graph)

  let update (node : Cfg.Node.t) sets =
    let { gen; kill; global_out; _ } = sets.(node.index) in
    match node.block.name with
    | "Exit" -> (S.union gen global_out, global_out)
    | _ -> (
        assert (Cfg.NodeSet.cardinal node.succ > 0);
        let out_set = Cfg.NodeSet.fold (fun s set ->
            T.meet sets.(s.index).global_in set
          ) node.succ (sets.((Cfg.NodeSet.choose node.succ).index).global_in)
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
      List.iter (fun node ->
          let in_set', out_set' = T.update node sets in
          if (in_set' <> sets.(node.index).global_in ||
              out_set' <> sets.(node.index).global_out) then
            changed := true;
          sets.(node.index).global_in <- in_set';
          sets.(node.index).global_out <- out_set'
        ) traversal;
      incr num_iter
    done;
    sets
end
