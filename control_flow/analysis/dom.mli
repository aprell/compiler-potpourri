open Cfg
open Utils

val dominators : cfg -> Nodes.t array

val immediate_dominators : cfg -> Nodes.t array

val back_edges : cfg -> (Nodes.elt * Nodes.elt) list
