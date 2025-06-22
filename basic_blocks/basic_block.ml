open Three_address_code__IR
open Three_address_code__Utils
open Utils

type t = {
  name : string;
  number : int;
  mutable stmts : stmt ref list;
  mutable pred : t list;
  mutable succ : t list;
}

let block_number = gen_number 1

(* Constructor for basic blocks *)
let create ?name ?(number = block_number ()) ?(stmts = []) () =
  { name = Option.value name ~default:("B" ^ string_of_int number);
    number; stmts; pred = []; succ = [] }

let compare a b = Stdlib.compare a.number b.number

let to_string block =
  Printf.sprintf "[%s]\n%s"
    block.name
    (block.stmts
     |> List.map (fun stmt -> string_of_stmt !stmt ~indent:4)
     |> String.concat "\n")

let first_stmt { stmts; _ } =
  match stmts with
  | stmt :: _ -> Some stmt
  | _ -> None

let last_stmt { stmts; _ } =
  match List.rev stmts with
  | stmt :: _ -> Some stmt
  | _ -> None

let entry_label block =
  match first_stmt block with
  | Some { contents = Label l } -> l
  | _ -> (block.name, None)

let create_basic_blocks source =
  List.fold_left (fun (lines, code, blocks) stmt ->
      match stmt with
      | Label (name, _) ->
        if lines = 0 then
          (* Extend basic block *)
          (lines + 1, code, blocks)
        else
          (* End previous basic block *)
          let stmts, code = split lines code in
          let stmts = List.map ref (peephole stmts) in
          let block = create () ~stmts:
              (* Insert explicit jump *)
              (stmts @ [ref (Jump (name, None))])
          in
          (* This line starts a new basic block *)
          (1, code, block :: blocks)
      | Jump _ | Cond _ | Return _ ->
        (* End current basic block *)
        let stmts, code = split (lines + 1) code in
        let stmts = List.map ref (peephole stmts) in
        let block = create () ~stmts in
        (* Next line starts a new basic block *)
        (0, code, block :: blocks)
      | _ ->
        (* Extend basic block *)
        (lines + 1, code, blocks)
    ) (0, source, []) source
  |> fun (_, code, blocks) ->
  if code <> [] then
    (* Close open basic block *)
    let stmts = List.map ref (peephole code) in
    let block = create () ~stmts:
        (* Insert explicit return *)
        (stmts @ [ref (Return None)])
    in
    List.rev (block :: blocks)
  else
    List.rev blocks

let print_basic_blocks blocks =
  List.map to_string blocks
  |> String.concat "\n"
  |> print_endline

let combine b1 b2 =
  let rec append xs ys =
    match xs, ys with
    | [{ contents = Jump _ }], _ -> append [] ys
    | [], { contents = Label _ } :: ys -> ys
    | x :: xs, ys -> x :: append xs ys
    | _ -> assert false
  in
  create ~number:(min b1.number b2.number) ~stmts:(append b1.stmts b2.stmts) ()

module Liveness = struct
  module Set = Vars

  let compute block =
    let rec loop (use, def) = function
      | stmt :: stmts ->
        let use', def' = match !stmt with
          | Move (x, e) ->
            let vars = Vars.of_expr e in
            (* Remove x from use, then add all variables that occur in e *)
            let use' = Set.union (Set.remove x use) vars in
            (* Add x to def, then remove all variables that occur in e *)
            let def' = Set.diff (Set.add x def) vars in
            (use', def')
          | Load (x, Mem (b, _)) ->
            (* Remove x from use, then add b *)
            let use' = Set.add b (Set.remove x use) in
            (* Add x to def, then remove b *)
            let def' = Set.remove b (Set.add x def) in
            (use', def')
          | Store (Mem (b, _), e) ->
            let vars = Vars.of_expr e in
            let use' = Set.add b (Set.union use vars) in
            (use', def)
          | Label (_, Some params) ->
            let vars = Set.of_list params in
            let use' = Set.diff use vars in
            let def' = Set.union def vars in
            (use', def')
          | Cond (e, _, _) | Return (Some e) ->
            let vars = Vars.of_expr e in
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
    loop (Set.empty, Set.empty) (List.rev block.stmts)
end
