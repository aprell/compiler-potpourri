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
