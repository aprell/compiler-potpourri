open Three_address_code__IR

val replace_stmt : var -> expr -> stmt -> stmt

val propagate : stmt -> stmt list -> stmt list

val optimize : stmt list -> stmt list
