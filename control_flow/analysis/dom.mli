open Cfg

val dominators : Cfg.t -> NodeSet.t array

val immediate_dominators : Cfg.t -> Node.t option array

module Domtree : sig
  type t
  type elt = Node.t
  val create : Cfg.t -> t
  val children : elt -> elt list
  val output_dot : ?filename:string -> t -> unit
  val inspect : t -> unit
end

val dominance_frontiers : Cfg.t -> Domtree.t -> NodeSet.t array
