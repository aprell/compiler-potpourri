open Three_address_code__IR

type t = {
  name : string;
  source : source_info option;
}

and source_info = {
  entry : string;
  exits : string list;
  stmts : stmt ref list;
  use : var list;
  def : var list;
}

val create : ?source:source_info -> string -> t

val to_string : t -> string

val update : t -> stmts:stmt ref list -> t

val create_basic_blocks : stmt list -> t list
