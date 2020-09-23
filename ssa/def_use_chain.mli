open Three_address_code__IR

module Set : Set.S with type elt = Basic_block.t ref * stmt ref ref

type t = {
  def : Set.elt option;
  uses : Set.t;
}

val get_def : var -> Set.elt option

val get_uses : var -> Set.t

val basic_block_of_def : var -> Basic_block.t ref option

val basic_blocks_of_uses : var -> Basic_block.t ref list

val add_def : Basic_block.t -> stmt ref -> var -> unit

val add_use : Basic_block.t -> stmt ref -> var -> unit

val remove_def : var -> unit

val remove_use : Set.elt -> var -> unit

val remove_uses : var -> unit

val build : Basic_block.t -> unit

val iter : (var -> Set.elt option -> Set.t -> unit) -> unit

val print : unit -> unit
