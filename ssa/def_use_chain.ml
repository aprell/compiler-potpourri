open Three_address_code__IR
open Basic_block
open Utils

module Set = Set.Make (struct
  type t = Basic_block.t ref * stmt ref ref
  let compare = Stdlib.compare
end)

(* When building semi-pruned SSA form, we may encounter label parameters that,
 * after renaming, lack a definition, but these variables will be removed
 * during SSA minimization. *)
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

let basic_block_of_def var =
  match get_def var with
  | Some (block, _) -> Some block
  | None -> None

let basic_blocks_of_uses var =
  get_uses var
  |> Set.elements
  |> List.map fst
  |> List.sort_uniq (fun block block' ->
      compare !block.name !block'.name
    )

let remove_use use var =
  match Hashtbl.find_opt def_use_chains var with
  | Some ({ uses; _ } as def_use_chain) -> (
      assert (uses <> Set.empty);
      Hashtbl.replace def_use_chains var
        { def_use_chain with uses = Set.remove use uses }
    )
  | None -> ()

let remove_uses ?(keep_phi_functions = true) var =
  let filter =
    if keep_phi_functions then
      Set.filter (fun (_, use) -> is_phi_function !(!use))
    else
      fun _ -> Set.empty
  in
  match Hashtbl.find_opt def_use_chains var with
  | Some ({ uses; _ } as def_use_chain) -> (
      Hashtbl.replace def_use_chains var
        { def_use_chain with uses = filter uses }
    )
  | None -> ()

let remove_def var =
  match Hashtbl.find_opt def_use_chains var with
  | Some { def; _ } -> (
      assert (Option.is_some def);
      let _, stmt as def = Option.get def in (
        match !(!stmt) with
        | Move (_, e) ->
          List.iter (remove_use def) (all_variables_expr e)
        | Load (_, Deref y) ->
          remove_use def y
        | Label (_, Some xs)
        | Phi (_, xs) ->
          List.iter (remove_use def) xs
        | _ -> assert false
      );
      Hashtbl.remove def_use_chains var
    )
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

let visit block stmt =
  match !stmt with
  | Move (x, e) ->
    add_def block stmt x;
    List.iter (add_use block stmt) (all_variables_expr e)
  | Load (x, Deref y) ->
    add_def block stmt x;
    add_use block stmt y
  | Store (Deref x, e) ->
    add_use block stmt x;
    List.iter (add_use block stmt) (all_variables_expr e)
  | Label (_, Some xs) ->
    List.iter (add_def block stmt) xs
  | Cond (e, _, _) ->
    List.iter (add_use block stmt) (all_variables_expr e)
  | Receive x ->
    add_def block stmt x
  | Return (Some e) ->
    List.iter (add_use block stmt) (all_variables_expr e)
  | Return None -> ()
  | Phi (x, xs) ->
    add_def block stmt x;
    List.iter (add_use block stmt) xs
  | _ -> ()

let build block =
  match block.source with
  | Some { stmts; _ } ->
    List.iter (visit block) stmts
  | None ->
    assert (block.name = "Entry" || block.name = "Exit")

let iter f =
  Hashtbl.iter (fun var { def; uses; } ->
      f var def uses
    ) def_use_chains

let to_string { def; uses; } =
  let string_of_stmt' (block, stmt) =
    Printf.sprintf "%s (%s)" (string_of_stmt !(!stmt)) !block.name
  in
  Printf.sprintf "%s"
    (Option.fold ~some:string_of_stmt' ~none:"---" def)
  ,
  Printf.sprintf "%s"
    (Set.elements uses |> List.map string_of_stmt' |> String.concat ", ")

let print () =
  Hashtbl.iter (fun (Var x) def_use_chain ->
      let def, uses = to_string def_use_chain in
      Printf.printf "%s: def = %s, uses = [%s]\n" x def uses
    ) def_use_chains
