open Three_address_code__IR

val replace_stmt : var -> expr -> stmt -> stmt

val propagate : stmt -> unit

val optimize : ?dump:bool -> Basic_block.t -> unit
