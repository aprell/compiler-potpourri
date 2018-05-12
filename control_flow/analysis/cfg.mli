open Utils

type cfg = node array

and node = {
  index : int;
  block : Basic.basic_block;
  mutable succ : IntSet.t;
  mutable pred : IntSet.t;
}

val define_cfg : nodes:int list -> edges:(int * int) list -> cfg
val construct_cfg : Basic.basic_block list -> cfg
val discard_source_info : cfg -> cfg
val equal : cfg -> cfg -> bool
