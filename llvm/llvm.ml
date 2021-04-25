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

let string_of_fun_decl { name; type_sig = (return, params); } =
  let return = string_of_ty return in
  let params = List.map string_of_ty params in
    Printf.sprintf
      "declare %s %%%s(%s)"
      return name (String.concat ", " params)
