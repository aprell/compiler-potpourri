(* Sparse simple constant propagation *)

open Three_address_code__IR
open Basic_block__Utils

(* Abstract values *)
type value =
  | Top          (* Undefined *)
  | Const of int (* A known constant *)
  | Bottom       (* Unknown; not a constant *)

let string_of_value = function
  | Top -> "Top"
  | Const n -> string_of_int n
  | Bottom -> "Bottom"

(* Combine two abstract values *)
let meet v1 v2 =
  match v1, v2 with
  | Top, any | any, Top -> any
  | Const n1, Const n2 when n1 = n2 -> Const n1
  | _ -> Bottom

(* Interpret op over abstract values v1 and v2 *)
let interpret op v1 v2 =
  match op, v1, v2 with
  | _, Top, _ | _, _, Top -> Top
  | Plus, Const n1, Const n2 -> Const (n1 + n2)
  | Minus, Const n1, Const n2 -> Const (n1 - n2)
  | Mul, Const n1, Const n2 -> Const (n1 * n2)
  | Div, Const n1, Const n2 -> Const (n1 / n2)
  | Mod, Const n1, Const n2 -> Const (n1 mod n2)
  | Mul, Bottom, Const 0 | Mul, Const 0, Bottom -> Const 0
  | _ -> Bottom

let string_of_binop = function
  | Plus -> "+"
  | Minus -> "-"
  | Mul -> "*"
  | Div -> "/"
  | Mod -> "%"

let meet' v1 v2 =
  let v3 = meet v1 v2 in
  Printf.printf "%s meet %s = %s\n"
    (string_of_value v1)
    (string_of_value v2)
    (string_of_value v3);
  v3

let interpret' op v1 v2 =
  let v3 = interpret op v1 v2 in
  Printf.printf "%s %s %s = %s\n"
    (string_of_value v1)
    (string_of_binop op)
    (string_of_value v2)
    (string_of_value v3);
  v3

(* Maps variables (SSA names) to abstract values *)
let values = Hashtbl.create 10

let value_of = Hashtbl.find values

let ( <-= ) var = Hashtbl.replace values var

let init ?(value = Top) () =
  let worklist = Queue.create () in
  Ssa__Def_use_chain.iter (fun x def _ -> (
        match def with
        | Some (_, stmt) -> (
            match !(!stmt) with
            | Move (_, Const n) -> x <-= Const n
            | Load _ -> x <-= Bottom
            | Label _ -> x <-= Bottom
            | Phi _ -> x <-= value
            | _ -> x <-= value
          )
        | None -> x <-= value
      );
      if value_of x <> Top then
        Queue.add x worklist
    );
  worklist

let propagate worklist = function
  | Move (x, e) ->
    let v = value_of x in
    if v <> Bottom then (
      begin match e with
        | Val y ->
          Printf.printf "%s := " (name_of_var x);
          x <-= value_of y;
          Printf.printf "%s\n" (string_of_value (value_of y))
        | Binop (op, Val y, Const n) ->
          Printf.printf "%s := " (name_of_var x);
          x <-= interpret' op (value_of y) (Const n)
        | Binop (op, Const n, Val y) ->
          Printf.printf "%s := " (name_of_var x);
          x <-= interpret' op (Const n) (value_of y)
        | Binop (op, Val y, Val z) ->
          Printf.printf "%s := " (name_of_var x);
          x <-= interpret' op (value_of y) (value_of z)
        | _ -> ()
      end;
      if value_of x <> v then
        Queue.add x worklist
    )
  | Phi (x, [y; z]) ->
    let v = value_of x in
    if v <> Bottom then (
      Printf.printf "%s := " (name_of_var x);
      x <-= meet' (value_of y) (value_of z);
      if value_of x <> v then
        Queue.add x worklist
    )
  | _ -> ()

let iterate worklist =
  while not (Queue.is_empty worklist) do
    let x = Queue.take worklist in
    let uses = Ssa__Def_use_chain.get_uses x in
    Ssa__Def_use_chain.Set.iter (fun (_, stmt) ->
        propagate worklist !(!stmt)
      ) uses
  done

let print () =
  let rows = Hashtbl.fold (fun var value rows ->
      [name_of_var var; string_of_value value] :: rows
    ) values []
  in
  print_table ~rows:(["Variable"; "Value"] :: List.sort compare rows)
