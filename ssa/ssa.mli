open Control_flow

val parameterize_labels : Cfg.t -> unit

val rename_variables : Cfg.t -> unit

val insert_phi_functions : Cfg.t -> unit
