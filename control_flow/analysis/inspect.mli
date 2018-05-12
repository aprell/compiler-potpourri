open Utils

val inspect :
  ?dom_sets:IntSet.t array ->
  ?idoms:IntSet.t array ->
  ?back_edges:(int * int) list ->
  Cfg.cfg ->
  unit
