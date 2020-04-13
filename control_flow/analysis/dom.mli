open Cfg

val dominators : cfg -> NodeSet.t array

val immediate_dominators : cfg -> Node.t option array

val back_edges : cfg -> (Node.t * Node.t) list
