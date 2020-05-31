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
  (* Line range *)
  source_loc : int * int;
}

(* Constructor for basic blocks *)
let create ?source name = { name; source; }

let add_line_numbers =
  List.map2 (Printf.sprintf "%3d %s")

let to_string ?(line_numbers = true) block =
  match block.source with
  | Some { stmts; source_loc = (a, b); _ } ->
    Printf.sprintf "[%s]\n%s"
      block.name
      (stmts
       |> List.map (string_of_stmt ~indent:4)
       |> (fun s -> if line_numbers then add_line_numbers (a -- b) s else s)
       |> String.concat "\n")
  | None -> block.name

let create_basic_blocks source =
  let gen_name = gen_sym "B" 1 in
  List.fold_left (fun (label, start, line, code, blocks) stmt ->
      match stmt with
      | Jump (target, _) ->
        (* End current basic block *)
        let stmts, code = split (line - start + 1) code in
        let block = create (gen_name ()) ~source:
            { entry = label;
              exits = [target];
              source_loc = (start, line);
              stmts; }
        in
        (* Next line starts a new basic block *)
        ("fall-through", line + 1, line + 1, code, block :: blocks)
      | Cond (_, (l1, _), (l2, _)) ->
        (* End current basic block *)
        let stmts, code = split (line - start + 1) code in
        let block = create (gen_name ()) ~source:
            { entry = label;
              exits = [l1; l2];
              source_loc = (start, line);
              stmts; }
        in
        (* Next line starts a new basic block *)
        ("fall-through", line + 1, line + 1, code, block :: blocks)
      | Return _ ->
        (* End current basic block *)
        let stmts, code = split (line - start + 1) code in
        let block = create (gen_name ()) ~source:
            { entry = label;
              exits = ["exit"];
              source_loc = (start, line);
              stmts; }
        in
        (* Next line starts a new basic block *)
        ("fall-through", line + 1, line + 1, code, block :: blocks)
      | Label (name, _) ->
        if start = line then
          (* Extend basic block *)
          (name, start, line + 1, code, blocks)
        else
          (* End previous basic block *)
          let stmts, code = split (line - start) code in
          let block = create (gen_name ()) ~source:
              { entry = label;
                exits = [name];
                (* Insert explicit jump (adds one line) *)
                source_loc = (start, line);
                stmts = stmts @ [Jump (name, None)]; }
          in
          (* This line starts a new basic block *)
          (name, line + 1, line + 2, code, block :: blocks)
      | _ ->
        (* Extend basic block *)
        (label, start, line + 1, code, blocks)
    ) ("entry", 1, 1, source, []) source
  |> fun (label, start, line, code, blocks) ->
  if code <> [] then
    (* Close open basic block with an implicit return *)
    let block = create (gen_name ()) ~source:
        { entry = label;
          exits = ["exit"];
          source_loc = (start, line - 1);
          stmts = code; }
    in
    List.rev (block :: blocks)
  else
    List.rev blocks
