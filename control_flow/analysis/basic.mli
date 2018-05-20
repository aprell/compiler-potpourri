type basic_block = Basic_block of IR.name * source_info option

and source_info = {
  entry : IR.name;
  exits : IR.name list;
  source_loc : int * int;
}

val basic_block : ?source_info:source_info -> name:IR.name -> basic_block

val to_string : basic_block -> string

val basic_blocks : IR.stmt list -> basic_block list
