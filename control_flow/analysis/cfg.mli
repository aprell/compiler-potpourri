module rec Node : sig
  type t = {
    block : Basic_block.t;
    mutable pred : NodeSet.t;
    mutable succ : NodeSet.t;
    mutable doms : NodeSet.t;
    mutable idom : Node.t option;
  }
end

and NodeSet : Set.S with type elt = Node.t

type t

val define : nodes:int list -> edges:(int * int) list -> t

val construct : Basic_block.t list -> t

val get_node : int -> t -> Node.t

val get_entry_node : t -> Node.t

val get_exit_node : t -> Node.t

val get_nodes : t -> Node.t list

val get_order : t -> int

val equal : t -> t -> bool

val iter : (Node.t -> unit) -> t -> unit

val dfs_reverse_postorder : t -> Node.t list

val dfs_postorder : t -> Node.t list

val print_basic_blocks : t -> unit

val output_dot : ?filename:string -> t -> unit

val inspect : t -> unit
