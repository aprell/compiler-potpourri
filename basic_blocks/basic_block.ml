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
  mutable stmts : stmt ref list;
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
          let block = create (gen_name ()) ~source:
              { entry = label;
                exits = [name];
                (* Insert explicit jump *)
                stmts = stmts @ [ref (Jump (name, None))]; }
          in
          (* This line starts a new basic block *)
          (name, 1, code, block :: blocks)
      | Jump _ | Cond _ | Return _ ->
        (* End current basic block *)
        let stmts, code = split (lines + 1) code in
        let stmts = List.map ref stmts in
        let block = create (gen_name ()) ~source:
            { entry = label;
              exits = jump_targets stmt;
              stmts; }
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
    let block = create (gen_name ()) ~source:
        { entry = label;
          exits = ["exit"];
          stmts; }
    in
    List.rev (block :: blocks)
  else
    List.rev blocks

module Liveness = struct
  module Set = Set.Make (struct
    type t = var
    let compare = Stdlib.compare
  end)

  let compute block =
    let rec loop (use, def) = function
      | stmt :: stmts ->
        let use', def' = match !stmt with
          | Move (x, e)
          | Load (x, Mem { offset = e; _ }) ->
            let vars = Set.of_list (all_variables_expr e) in
            (* Remove x from use, then add all variables that occur in e *)
            let use' = Set.union (Set.remove x use) vars in
            (* Add x to def, then remove all variables that occur in e *)
            let def' = Set.diff (Set.add x def) vars in
            (use', def')
          | Store (_, e) ->
            let vars = Set.of_list (all_variables_expr e) in
            let use' = Set.union use vars in
            (use', def)
          | Label (_, Some params) ->
            let vars = Set.of_list params in
            let use' = Set.diff use vars in
            let def' = Set.union def vars in
            (use', def')
          | Cond (e, _, _) | Return (Some e) ->
            let vars = Set.of_list (all_variables_expr e) in
            let use' = Set.union use vars in
            (use', def)
          | Phi (x, xs) ->
            let vars = Set.of_list xs in
            let use' = Set.union (Set.remove x use) vars in
            let def' = Set.diff (Set.add x def) vars in
            (use', def')
          (* Incomplete; extend as needed *)
          | _ -> (use, def)
        in
        loop (use', def') stmts
      | [] ->
        assert (Set.is_empty (Set.inter use def));
        (use, def)
    in
    match block.source with
    | Some { stmts; _ } ->
      loop (Set.empty, Set.empty) (List.rev stmts)
    | None ->
      (Set.empty, Set.empty)
end
