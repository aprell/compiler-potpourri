open Three_address_code__IR
open Control_flow

val parameterize_labels : Cfg.t -> unit

val rename_variables : Cfg.t -> unit

val insert_phi_functions : Cfg.t -> unit

val minimize_phi_functions : Cfg.t -> unit

module Graph : sig
  type t

  type def_use = def * use_def list
  and use_def = use * def
  and def = Def_use_chain.Set.elt
  and use = Def_use_chain.Set.elt

  val create : unit -> t
  val get_def_use : var -> t -> def_use option
  val get_def : var -> t -> def option
  val get_use_def : var -> t -> use_def list
  val get_uses : var -> t -> use list
  val add_use : var -> use -> t -> unit
  val set_uses : var -> use list -> t -> unit
  val remove_use : var -> use -> t -> unit
  val remove_uses : var -> t -> unit
  val remove_def : var -> t -> unit
  val iter : (var -> def_use -> unit) -> t -> unit
  val find_first : (def_use -> bool) -> t -> (var * def_use) option
  val print : t -> unit
  val output_dot : ?filename:string -> t -> unit
end

val convert_to_ssa : Cfg.t -> Graph.t
