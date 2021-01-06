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

type t = Node.t array

val define : nodes:int list -> edges:(int * int) list -> t

val construct : Basic_block.t list -> t

val basic_blocks : t -> Basic_block.t list

val print_basic_blocks : t -> unit

val equal : t -> t -> bool

val dfs_reverse_postorder : t -> Node.t list

val dfs_postorder : t -> Node.t list

val unreachable : Node.t -> bool

val iter : (Node.t -> unit) -> t -> unit

val output_dot : ?filename:string -> t -> unit

val inspect : t -> unit
