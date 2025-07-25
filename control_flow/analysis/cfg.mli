module rec Node : sig
  type t = {
    block : Basic_block.t;
    mutable pred : NodeSet.t;
    mutable succ : NodeSet.t;
    mutable doms : NodeSet.t;
    mutable idom : Node.t option;
  }

  (* Add an edge from node a to node b *)
  val ( => ) : t -> t -> unit

  (* Remove the edge between node a and node b *)
  val ( =|> ) : t -> t -> unit

  val combine : t -> t -> t
end

and NodeSet : Set.S with type elt = Node.t

module IntSet : Set.S with type elt = int

type t

val define : nodes:int list -> edges:(int * int) list -> t

val construct : Basic_block.t list -> t

val add_node : Node.t -> t -> t

val add_nodes : NodeSet.t -> t -> t

val remove_node : Node.t -> t -> t

val get_node : int -> t -> Node.t

val get_node_opt : int -> t -> Node.t option

val get_entry_node : t -> Node.t

val get_exit_node : t -> Node.t option

val get_nodes : t -> Node.t list

val get_order : t -> int

val equal : t -> t -> bool

val iter : (Node.t -> unit) -> t -> unit

val filter : (int -> Node.t -> bool) -> t -> t

val dfs_reverse_postorder : t -> int list

val dfs_postorder : t -> int list

val get_first_basic_block : t -> Basic_block.t

val get_basic_blocks : t -> Basic_block.t list

val print_basic_blocks : t -> unit

val output_dot : ?filename:string -> t -> unit

val inspect : t -> unit

val split_edge : Node.t * Node.t -> Node.t

val is_critical_edge : Node.t * Node.t -> bool

val split_critical_edges : t -> t
