open Three_address_code__IR
open Three_address_code__Utils
open Utils

type t = {
  name : string;
  source : source_info option;
}

and source_info = {
  entry : string;
  exits : string list;
  stmts : stmt ref list;
  use : var list;
  def : var list;
}

(* Constructor for basic blocks *)
let create ?source name = { name; source; }

let to_string block =
  match block.source with
  | Some { stmts; _ } ->
    Printf.sprintf "[%s]\n%s"
      block.name
      (stmts
       |> List.map (fun stmt -> string_of_stmt !stmt ~indent:4)
       |> String.concat "\n")
  | None -> block.name

let jump_targets = function
  | Jump (target, _) -> [target]
  | Cond (_, (then_, _), (else_, _)) -> [then_; else_]
  | Return _ -> ["exit"]
  | _ -> []

module S = Set.Make (struct
  type t = var
  let compare = Stdlib.compare
end)

(* Block-local liveness information
 * use: the set of variables that are used before being assigned a (new) value
 * def: the set of variables that are assigned a (new) value before being used
 *)
let use_def stmts =
  let rec loop (use, def) = function
    | stmt :: stmts ->
      let use', def' = match !stmt with
        | Move (x, e) ->
          let vars = S.of_list (all_variables_expr e) in
          (* Remove x from use, then add all variables that occur in e *)
          let use' = S.union (S.remove x use) vars in
          (* Add x to def, then remove all variables that occur in e *)
          let def' = S.diff (S.add x def) vars in
          (use', def')
        | Label (_, Some params) ->
          let vars = S.of_list params in
          let use' = S.diff use vars in
          let def' = S.union def vars in
          (use', def')
        | Cond (e, _, _) | Return (Some e) ->
          let vars = S.of_list (all_variables_expr e) in
          let use' = S.union use vars in
          (use', def)
        | Phi (x, xs) ->
          let vars = S.of_list xs in
          let use' = S.union (S.remove x use) vars in
          let def' = S.diff (S.add x def) vars in
          (use', def')
        (* Incomplete; extend as needed *)
        | _ -> (use, def)
      in
      loop (use', def') stmts
    | [] ->
      assert (S.is_empty (S.inter use def));
      (S.elements use, S.elements def)
  in
  loop (S.empty, S.empty) (List.rev stmts)

let update block ~stmts =
  let use, def = use_def stmts in
  match block.source with
  | Some info ->
    create block.name ~source:
      { info with stmts; use; def; }
  | None ->
    invalid_arg "Basic block lacks source information"

let create_basic_blocks source =
  let gen_name = gen_sym "B" 1 in
  List.fold_left (fun (label, lines, code, blocks) stmt ->
      match stmt with
      | Label (name, _) ->
        if lines = 0 then
          (* Extend basic block *)
          (name, lines + 1, code, blocks)
        else
          (* End previous basic block *)
          let stmts, code = split lines code in
          let stmts = List.map ref stmts in
          let use, def = use_def stmts in
          let block = create (gen_name ()) ~source:
              { entry = label;
                exits = [name];
                (* Insert explicit jump *)
                stmts = stmts @ [ref (Jump (name, None))];
                use; def; }
          in
          (* This line starts a new basic block *)
          (name, 1, code, block :: blocks)
      | Jump _ | Cond _ | Return _ ->
        (* End current basic block *)
        let stmts, code = split (lines + 1) code in
        let stmts = List.map ref stmts in
        let use, def = use_def stmts in
        let block = create (gen_name ()) ~source:
            { entry = label;
              exits = jump_targets stmt;
              stmts; use; def; }
        in
        (* Next line starts a new basic block *)
        ("next", 0, code, block :: blocks)
      | _ ->
        (* Extend basic block *)
        (label, lines + 1, code, blocks)
    ) ("entry", 0, source, []) source
  |> fun (label, _, code, blocks) ->
  if code <> [] then
    (* Close open basic block with an implicit return *)
    let stmts = List.map ref code in
    let use, def = use_def stmts in
    let block = create (gen_name ()) ~source:
        { entry = label;
          exits = ["exit"];
          stmts; use; def; }
    in
    List.rev (block :: blocks)
  else
    List.rev blocks
