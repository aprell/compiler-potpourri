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
  val update : Cfg.Node.t -> Set.t analysis array -> Set.t * Set.t
end

module Forward_flow (A : AnalysisType) = struct
  include A

  let traverse = Cfg.dfs_reverse_postorder

  let update (node : Cfg.Node.t) sets =
    let { gen; kill; global_in; _ } = sets.(node.index) in
    match node.block.name with
    | "Entry" -> (global_in, Set.union gen global_in)
    | _ -> (
        assert (Cfg.NodeSet.cardinal node.pred > 0);
        let in_set = Cfg.NodeSet.fold (fun p set ->
            A.meet sets.(p.index).global_out set
          ) node.pred (sets.((Cfg.NodeSet.choose node.pred).index).global_out)
        in
        let out_set = Set.(union gen (diff in_set kill)) in
        (in_set, out_set)
      )
end

module Backward_flow (A : AnalysisType) = struct
  include A

  let traverse = Cfg.dfs_postorder

  let update (node : Cfg.Node.t) sets =
    let { gen; kill; global_out; _ } = sets.(node.index) in
    match node.block.name with
    | "Exit" -> (Set.union gen global_out, global_out)
    | _ -> (
        assert (Cfg.NodeSet.cardinal node.succ > 0);
        let out_set = Cfg.NodeSet.fold (fun s set ->
            A.meet sets.(s.index).global_in set
          ) node.succ (sets.((Cfg.NodeSet.choose node.succ).index).global_in)
        in
        let in_set = Set.(union gen (diff out_set kill)) in
        (in_set, out_set)
      )
end

module Data_flow_analysis (DF : DataFlowType) = struct
  let print_gen_kill sets =
    let column_widths = [5; 20; 20] in
    let rows =
      Array.fold_left (fun rows { gen; kill; _ } ->
          [ DF.Set.to_string gen;
            DF.Set.to_string kill ] :: rows
        ) [] sets
      |> List.rev
      |> List.map2 (fun x y -> x :: y)
        ("Entry" :: ("B" ^^ (1 -- (Array.length sets - 2))) @ ["Exit"])
    in
    print_table
      ~column_widths
      ~rows:([""; "gen"; "kill"] :: rows)

  let print_in_out sets =
    let column_widths = [5; 20; 20] in
    let rows =
      Array.fold_left (fun rows { global_in; global_out; _ } ->
          [ DF.Set.to_string global_in;
            DF.Set.to_string global_out ] :: rows
        ) [] sets
    |> List.rev
    |> List.map2 (fun x y -> x :: y)
      ("Entry" :: ("B" ^^ (1 -- (Array.length sets - 2))) @ ["Exit"])
    in
    print_table
      ~column_widths
      ~rows:([""; "IN"; "OUT"] :: rows)

  let init graph =
    let num_nodes = Array.length graph in
    Array.init num_nodes (fun i -> DF.init graph.(i) graph)

  let compute ?(dump = false) graph =
    let traversal = DF.traverse graph in
    let sets = init graph in
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
          if (not (DF.Set.equal in_set' sets.(node.index).global_in) ||
              not (DF.Set.equal out_set' sets.(node.index).global_out)) then
            changed := true;
          sets.(node.index).global_in <- in_set';
          sets.(node.index).global_out <- out_set'
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
