open Three_address_code__IR

type basic_block = Basic_block of name * source_info option

and source_info = {
  entry : name;
  exits : name list;
  stmts : stmt list;
  source_loc : int * int;
}

val basic_block : ?source_info:source_info -> name -> basic_block

val to_string : basic_block -> string

val basic_blocks : stmt list -> basic_block list
