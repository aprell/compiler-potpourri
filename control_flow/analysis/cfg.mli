open Basic
open Utils

type cfg = node array

and node = {
  index : Nodes.elt;
  block : basic_block;
  mutable succ : Nodes.t;
  mutable pred : Nodes.t;
}

val define_cfg : nodes:Nodes.elt list -> edges:(Nodes.elt * Nodes.elt) list -> cfg

val construct_cfg : basic_block list -> cfg

val discard_source_info : cfg -> cfg

val equal : cfg -> cfg -> bool
