open Three_address_code__IR

type t = {
  name : string;
  source : source_info option;
}

and source_info = {
  entry : string;
  exits : string list;
  stmts : stmt ref list;
}

val create : ?source:source_info -> string -> t

val to_string : t -> string

val update : t -> stmts:stmt ref list -> t

val create_basic_blocks : stmt list -> t list

module Liveness : sig
  module Set : Set.S with type elt = var

  (* Block-local liveness information
   * use: the set of variables that are used before being assigned a (new) value
   * def: the set of variables that are assigned a (new) value before being used
   *)
  val compute : t -> (* use *) Set.t * (* def *) Set.t
end
