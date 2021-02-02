open Three_address_code__IR

type t = {
  name : string;
  number : int;
  mutable stmts : stmt ref list;
  mutable pred : t list;
  mutable succ : t list;
}

val create : ?name:string -> ?stmts:stmt ref list -> int -> t

val compare : t -> t -> int

val to_string : t -> string

val entry_label : t -> label

val first_stmt : t -> stmt ref option

val last_stmt : t -> stmt ref option

val create_basic_blocks : stmt list -> t list

val print_basic_blocks : t list -> unit

module Liveness : sig
  module Set = Vars

  (* Block-local liveness information
   * use: the set of variables that are used before being assigned a (new) value
   * def: the set of variables that are assigned a (new) value before being used
   *)
  val compute : t -> (* use *) Set.t * (* def *) Set.t
end
