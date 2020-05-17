open Three_address_code__IR

val propagate : stmt -> stmt list -> stmt list

val optimize : stmt list -> stmt list
