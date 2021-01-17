open Three_address_code__IR
open Control_flow

val propagate_phi : var -> var -> unit

val propagate_const : var -> int -> unit

val propagate_copy : var -> var -> unit

val optimize : ?dump:bool -> Cfg.t -> Cfg.t
