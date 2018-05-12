open Utils

val dominators : Cfg.cfg -> IntSet.t array
val immediate_dominators : Cfg.cfg -> IntSet.t array -> IntSet.t array
val back_edges : Cfg.cfg -> IntSet.t array -> (int * int) list
