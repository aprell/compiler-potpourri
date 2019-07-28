val input_lines : in_channel -> string list

val read_file : string -> string list

val unlines : string list -> string

val ( -- ) : int -> int -> int list

val ( ^^ ) : string -> int list -> string list

val take : int -> 'a list -> 'a list

val drop : int -> 'a list -> 'a list

val split : int -> 'a list -> 'a list * 'a list

val sublist : int -> int -> 'a list -> 'a list

module Nodes : sig
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
