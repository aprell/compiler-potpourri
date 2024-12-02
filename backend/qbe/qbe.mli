open Three_address_code
open Control_flow

val emit_function : IR.decl -> Cfg.t -> unit

val emit : ?optimize:bool -> string -> unit
