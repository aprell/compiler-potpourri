val dominators : Cfg.cfg -> Cfg.IntSet.t array
val immediate_dominators : Cfg.cfg -> Cfg.IntSet.t array -> Cfg.IntSet.t array
val back_edges : Cfg.cfg -> Cfg.IntSet.t array -> (int * int) list
