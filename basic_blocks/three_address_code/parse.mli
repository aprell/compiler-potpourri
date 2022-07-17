val parse_line : string -> IR.stmt

val parse_prog : string -> IR.decl option * IR.stmt list

val parse_file : string -> IR.decl option * IR.stmt list
