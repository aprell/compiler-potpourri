open Three_address_code__IR
open Basic_block

module Set = Set.Make (struct
  type t = Basic_block.t ref * stmt ref ref
  let compare = Stdlib.compare
end)

(* Variables that are used as basic block parameters may introduce SSA names
 * for which no definitions exist. *)

type t = {
  def : Set.elt option;
  uses : Set.t;
}

let def_use_chains = Hashtbl.create 10

let get_def var =
  match Hashtbl.find_opt def_use_chains var with
  | Some { def; _ } -> def
  | None -> None

let get_uses var =
  match Hashtbl.find_opt def_use_chains var with
  | Some { uses; _ } -> uses
  | None -> Set.empty

let remove_use use var =
  match Hashtbl.find_opt def_use_chains var with
  | Some ({ uses; _ } as def_use_chain) ->
    assert (uses <> Set.empty);
    Hashtbl.replace def_use_chains var
      { def_use_chain with uses = Set.remove use uses }
  | None -> ()

let remove_uses var =
  match Hashtbl.find_opt def_use_chains var with
  | Some def_use_chain ->
    Hashtbl.replace def_use_chains var
      { def_use_chain with uses = Set.empty }
  | None -> ()

let remove_def var =
  match Hashtbl.find_opt def_use_chains var with
  | Some { def = Some ((_, stmt) as def); uses } -> (
      match !(!stmt) with
      | Move (_, e) ->
        Vars.iter (remove_use def) (collect_variables e)
      | Load (_, Mem (b, Val o)) ->
        remove_use def b;
        remove_use def o
      | Load (_, Mem (b, Const _)) ->
        remove_use def b
      | Label (_, Some xs)
      | Phi (_, xs) ->
        List.iter (remove_use def) xs
      | _ -> assert false
    );
    Hashtbl.replace def_use_chains var
      { def = None; uses }
  | Some { def = None; _ }
  | None -> ()

let add_def block stmt var =
  match Hashtbl.find_opt def_use_chains var with
  | Some ({ def; _ } as def_use_chain) ->
    assert (Option.is_none def);
    Hashtbl.replace def_use_chains var
      { def_use_chain with def = Some (ref block, ref stmt) }
  | None ->
    Hashtbl.add def_use_chains var
      { def = Some (ref block, ref stmt); uses = Set.empty; }

let add_use block stmt var =
  match Hashtbl.find_opt def_use_chains var with
  | Some ({ uses; _ } as def_use_chain) ->
    Hashtbl.replace def_use_chains var
      { def_use_chain with uses = Set.add (ref block, ref stmt) uses }
  | None ->
    Hashtbl.add def_use_chains var
      { def = None; uses = Set.singleton (ref block, ref stmt); }

let build block =
  let visit stmt =
    match !stmt with
    | Move (x, e) ->
      add_def block stmt x;
      Vars.iter (add_use block stmt) (collect_variables e)
    | Load (x, Mem (b, Val o)) ->
      add_def block stmt x;
      add_use block stmt b;
      add_use block stmt o
    | Load (x, Mem (b, Const _)) ->
      add_def block stmt x;
      add_use block stmt b
    | Store (Mem (b, Val o), e) ->
      add_use block stmt b;
      add_use block stmt o;
      Vars.iter (add_use block stmt) (collect_variables e)
    | Store (Mem (b, Const _), e) ->
      add_use block stmt b;
      Vars.iter (add_use block stmt) (collect_variables e)
    | Label (_, Some xs) ->
      List.iter (add_def block stmt) xs
    | Cond (e, _, _) ->
      Vars.iter (add_use block stmt) (collect_variables e)
    | Return (Some e) ->
      Vars.iter (add_use block stmt) (collect_variables e)
    | Return None -> ()
    | Phi (x, xs) ->
      add_def block stmt x;
      List.iter (add_use block stmt) xs
    | _ -> ()
  in
  List.iter visit block.stmts

let iter f =
  Hashtbl.iter (fun var { def; uses; } ->
      f var def uses
    ) def_use_chains

let clear () =
  Hashtbl.reset def_use_chains
