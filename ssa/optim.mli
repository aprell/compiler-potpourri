open Three_address_code__IR

val propagate_phi : var -> var -> unit

val propagate_const : var -> int -> unit

val propagate_copy : var -> var -> unit

val propagate : stmt -> unit

val optimize : ?dump:bool -> unit -> unit
