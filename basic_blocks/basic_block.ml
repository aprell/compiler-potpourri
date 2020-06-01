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
  stmts : stmt list;
}

(* Constructor for basic blocks *)
let create ?source name = { name; source; }

let to_string block =
  match block.source with
  | Some { stmts; _ } ->
    Printf.sprintf "[%s]\n%s"
      block.name
      (stmts
       |> List.map (string_of_stmt ~indent:4)
       |> String.concat "\n")
  | None -> block.name

let create_basic_blocks source =
  let gen_name = gen_sym "B" 1 in
  let jump_targets = function
    | Jump (target, _) -> [target]
    | Cond (_, (then_, _), (else_, _)) -> [then_; else_]
    | Return _ -> ["exit"]
    | _ -> []
  in
  List.fold_left (fun (label, lines, code, blocks) stmt ->
      match stmt with
      | Label (name, _) ->
        if lines = 0 then
          (* Extend basic block *)
          (name, lines + 1, code, blocks)
        else
          (* End previous basic block *)
          let stmts, code = split lines code in
          let block = create (gen_name ()) ~source:
              { entry = label;
                exits = [name];
                (* Insert explicit jump *)
                stmts = stmts @ [Jump (name, None)]; }
          in
          (* This line starts a new basic block *)
          (name, 1, code, block :: blocks)
      | Jump _ | Cond _ | Return _ ->
        (* End current basic block *)
        let stmts, code = split (lines + 1) code in
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
    let block = create (gen_name ()) ~source:
        { entry = label;
          exits = ["exit"];
          stmts = code; }
    in
    List.rev (block :: blocks)
  else
    List.rev blocks
