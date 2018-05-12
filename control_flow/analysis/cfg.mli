module IntSet :
  sig
    type elt = int
    type t
    val empty : t
    val is_empty : t -> bool
    val mem : elt -> t -> bool
    val add : elt -> t -> t
    val singleton : elt -> t
    val remove : elt -> t -> t
    val union : t -> t -> t
    val inter : t -> t -> t
    val diff : t -> t -> t
    val compare : t -> t -> int
    val equal : t -> t -> bool
    val subset : t -> t -> bool
    val iter : (elt -> unit) -> t -> unit
    val map : (elt -> elt) -> t -> t
    val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
    val for_all : (elt -> bool) -> t -> bool
    val exists : (elt -> bool) -> t -> bool
    val filter : (elt -> bool) -> t -> t
    val partition : (elt -> bool) -> t -> t * t
    val cardinal : t -> int
    val elements : t -> elt list
    val min_elt : t -> elt
    val max_elt : t -> elt
    val choose : t -> elt
    val split : elt -> t -> t * bool * t
    val find : elt -> t -> elt
    val of_list : elt list -> t
  end

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

val inspect :
  ?dom_sets:IntSet.t array ->
  ?idoms:IntSet.t array ->
  ?back_edges:(int * int) list ->
  cfg ->
  unit
