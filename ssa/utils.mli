open Three_address_code__IR

val ( >> ) : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c

val ( !! ) : stmt ref ref -> stmt

val is_phi_function : stmt -> bool
