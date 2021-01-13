open Cfg

val dominators : Cfg.t -> unit

val immediate_dominators : Cfg.t -> unit

module Domtree : sig
  type t
  val create : Cfg.t -> t
  val output_dot : ?filename:string -> t -> unit
end

val dominance_frontiers : Cfg.t -> Domtree.t -> NodeSet.t array
