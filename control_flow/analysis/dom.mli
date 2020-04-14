open Cfg

val dominators : Cfg.t -> NodeSet.t array

val immediate_dominators : Cfg.t -> Node.t option array

val back_edges : Cfg.t -> (Node.t * Node.t) list
