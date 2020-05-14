open Basic

module rec Node : sig
  type t = {
    index : int;
    mutable block : basic_block;
    mutable succ : NodeSet.t;
    mutable pred : NodeSet.t;
    mutable doms : NodeSet.t;
    mutable idom : Node.t option;
  }
end

and NodeSet : Set.S with type elt = Node.t

type t = Node.t array

val define : nodes:int list -> edges:(int * int) list -> t

val construct : basic_block list -> t

val basic_blocks : t -> basic_block list

val discard_source_info : t -> t

val equal : t -> t -> bool

val dfs_reverse_postorder : t -> Node.t list

val dfs_postorder : t -> Node.t list

val unreachable : Node.t -> bool

val output_dot : ?filename:string -> t -> unit

val inspect : ?back_edges:(Node.t * Node.t) list -> t -> unit
