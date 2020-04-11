open Basic
open Utils

type cfg = node array

and node = {
  index : Nodes.elt;
  block : basic_block;
  mutable succ : Nodes.t;
  mutable pred : Nodes.t;
  mutable doms : Nodes.t;
  mutable idom : Nodes.elt option;
}

val define_cfg : nodes:Nodes.elt list -> edges:(Nodes.elt * Nodes.elt) list -> cfg

val construct_cfg : basic_block list -> cfg

val discard_source_info : cfg -> cfg

val equal : cfg -> cfg -> bool

val dfs_reverse_postorder : cfg -> Nodes.elt list

val dfs_postorder : cfg -> Nodes.elt list

val unreachable : node -> bool

val output_dot : ?filename:string -> cfg -> unit
