open Cfg
open Utils

val inspect :
  ?dom_sets:Nodes.t array ->
  ?idoms:Nodes.t array ->
  ?back_edges:(Nodes.elt * Nodes.elt) list ->
  cfg ->
  unit
