open Cfg

type back_edge = Node.t * Node.t

type t = {
  head : Node.t;
  tail : Node.t;
  nodes : NodeSet.t;
}

val back_edges : Cfg.t -> back_edge list

val find : back_edge -> t

val print : t -> unit

module NestingForest : sig
  type t
  type elt = Node.t
  val create : Cfg.t -> t
  val output_dot : ?filename:string -> t -> unit
  val inspect : t -> unit
end
