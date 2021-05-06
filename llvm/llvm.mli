open Control_flow

type fun_decl

type ty = Int32 | Ptr of ty | Void

val declare : ?return:ty -> string -> params:ty list -> fun_decl

val emit_function_declaration : fun_decl -> unit

val emit_function : Cfg.t -> fun_decl -> unit

module Test : sig
  val emit : ?optimize:bool -> string -> unit
end
