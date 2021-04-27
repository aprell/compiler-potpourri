open Control_flow

type fun_decl

type ty = Int32 | Ptr of ty | Void

val declare : ?return:ty -> string -> params:ty list -> fun_decl

val string_of_fun_decl : fun_decl -> string

val print : Cfg.t -> fun_decl -> unit
