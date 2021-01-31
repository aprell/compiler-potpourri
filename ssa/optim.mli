open Control_flow

val optimize : ?dump:bool -> Cfg.t -> Ssa.Graph.t -> Cfg.t
