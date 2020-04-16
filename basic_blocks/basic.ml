open Three_address_code__IR
open Three_address_code__Utils
open Utils

type basic_block = Basic_block of name * source_info option

and source_info = {
  entry : name;
  exits : name list;
  stmts : stmt list;
  (* Line range *)
  source_loc : int * int;
}

(* Constructor for basic blocks *)
let create ?source_info name =
  Basic_block (name, source_info)

let add_line_numbers =
  List.map2 (Printf.sprintf "%3d %s")

let to_string (Basic_block (name, source_info)) : string =
  match source_info with
  | Some { stmts; source_loc = (a, b); _ } ->
    Printf.sprintf "[%s]\n%s"
      name
      (stmts
       |> List.map (string_of_stmt ~indent:4)
       |> add_line_numbers (a -- b)
       |> String.concat "\n")
  | None -> name

let create_basic_blocks (code : stmt list) : basic_block list =
  let gen_name = gen_sym "B" 1 in
  List.fold_left (fun (label, start, line, code, blocks) stmt ->
      match stmt with
      | Jump target ->
        (* End current basic block *)
        let stmts, code = split (line - start + 1) code in
        let block = create (gen_name ()) ~source_info:
            { entry = label;
              exits = [target];
              source_loc = (start, line);
              stmts; }
        in
        (* Next line starts a new basic block *)
        ("fall-through", line + 1, line + 1, code, block :: blocks)
      | Cond (_, target) ->
        (* End current basic block *)
        let stmts, code = split (line - start + 1) code in
        let block = create (gen_name ()) ~source_info:
            { entry = label;
              exits = [target; "fall-through"];
              source_loc = (start, line);
              stmts; }
        in
        (* Next line starts a new basic block *)
        ("fall-through", line + 1, line + 1, code, block :: blocks)
      | Return _ ->
        (* End current basic block *)
        let stmts, code = split (line - start + 1) code in
        let block = create (gen_name ()) ~source_info:
            { entry = label;
              exits = ["exit"];
              source_loc = (start, line);
              stmts; }
        in
        (* Next line starts a new basic block *)
        ("fall-through", line + 1, line + 1, code, block :: blocks)
      | Label lab ->
        if start = line then
          (* Extend basic block *)
          (lab, start, line + 1, code, blocks)
        else
          (* End previous basic block *)
          let stmts, code = split (line - start) code in
          let block = create (gen_name ()) ~source_info:
              { entry = label;
                exits = [lab];
                source_loc = (start, line - 1);
                stmts; }
          in
          (* This line starts a new basic block *)
          (lab, line, line + 1, code, block :: blocks)
      | _ ->
        (* Extend basic block *)
        (label, start, line + 1, code, blocks)
    ) ("entry", 1, 1, code, []) code
  |> fun (label, start, line, code, blocks) ->
  if code <> [] then
    (* Close open basic block with an implicit return *)
    let block = create (gen_name ()) ~source_info:
        { entry = label;
          exits = ["exit"];
          source_loc = (start, line - 1);
          stmts = code; }
    in
    List.rev (block :: blocks)
  else
    List.rev blocks
