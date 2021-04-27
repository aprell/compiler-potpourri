open Three_address_code__IR
open Control_flow

type fun_decl = {
  name : string;
  type_sig : ty * ty list;
}

and ty = Int32 | Ptr of ty | Void

(* Constructor for function declarations *)
let declare ?(return = Void) name ~params =
  { name; type_sig = (return, params); }

let rec string_of_ty = function
  | Int32 -> "i32"
  | Ptr x -> string_of_ty x ^ "*"
  | Void -> "void"

let global name = "@" ^ name

let local name = "%" ^ name

let string_of_fun_decl { name; type_sig = (return, params); } =
  let return = string_of_ty return in
  let params = List.map string_of_ty params in
    Printf.sprintf
      "declare %s %s(%s)"
      return (global name) (String.concat ", " params)

let get_first_basic_block (graph : Cfg.t) =
  let open Cfg in
  let entry = get_entry_node graph in
  assert (NodeSet.cardinal entry.succ = 1);
  (NodeSet.choose entry.succ).block

let print (graph : Cfg.t) (decl : fun_decl) =
  match Basic_block.entry_label (get_first_basic_block graph) with
  | name, Some params ->
    let ty, tys = decl.type_sig in
    if name = decl.name && List.length params = List.length tys then (
      let params = List.map2 (fun param ty ->
          Printf.sprintf
            "%s %s"
            (string_of_ty ty) (local (name_of_var param))
        ) params tys
      in
      Printf.printf
        "define %s %s(%s) {}\n"
        (string_of_ty ty) (global name) (String.concat ", " params)
    ) else (
      failwith "Function signature mismatch"
    )
  | _ -> failwith "TODO"
