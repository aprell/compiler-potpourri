val input_lines : in_channel -> string list

val read_file : string -> string list

val read_file_into_string : string -> string

val printf : ?indent:int -> ('a, out_channel, unit) format -> 'a

val sprintf : ?indent:int -> ('a, unit, string) format -> 'a

val gen_number : int -> unit -> int

val gen_name : string -> int -> ?pref:string -> unit -> string

val ( >> ) : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c
