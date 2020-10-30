open Three_address_code__IR

val value_numbers : (name, int) Hashtbl.t

(* An expression is available and need not be recomputed if it has a value
 * number that is bound to a variable *)

val available_exprs : (int, var) Hashtbl.t

val value_number : expr -> int
