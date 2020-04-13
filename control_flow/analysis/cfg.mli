open Basic

module rec Node : sig
  type t = {
    index : int;
    block : basic_block;
    mutable succ : NodeSet.t;
    mutable pred : NodeSet.t;
    mutable doms : NodeSet.t;
    mutable idom : Node.t option;
  }
end

and NodeSet : Set.S with type elt = Node.t

type cfg = Node.t array

val define_cfg : nodes:int list -> edges:(int * int) list -> cfg

val construct_cfg : basic_block list -> cfg

val discard_source_info : cfg -> cfg

val equal : cfg -> cfg -> bool

val dfs_reverse_postorder : cfg -> Node.t list

val dfs_postorder : cfg -> Node.t list

val unreachable : Node.t -> bool

val output_dot : ?filename:string -> cfg -> unit
