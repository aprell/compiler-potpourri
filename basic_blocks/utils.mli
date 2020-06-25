val ( -- ) : int -> int -> int list

val ( ^^ ) : string -> int list -> string list

val take : int -> 'a list -> 'a list

val drop : int -> 'a list -> 'a list

val split : int -> 'a list -> 'a list * 'a list

val sublist : int -> int -> 'a list -> 'a list

val pad : string -> int -> string

val print_table : column_widths:int list -> rows:string list list -> unit
