(* Sparse simple constant propagation *)

open Three_address_code__IR
open Basic_block__Utils

let verbose_flag = ref false

let printf fmt =
  if !verbose_flag then Printf.fprintf stdout fmt
  else Printf.ifprintf stdout fmt

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
  printf "%s meet %s = %s\n"
    (string_of_value v1)
    (string_of_value v2)
    (string_of_value v3);
  v3

let interpret' op v1 v2 =
  let v3 = interpret op v1 v2 in
  printf "%s %s %s = %s\n"
    (string_of_value v1)
    (string_of_binop op)
    (string_of_value v2)
    (string_of_value v3);
  v3

(* Maps variables (SSA names) to abstract values *)
let values = Hashtbl.create 10

let value_of = Hashtbl.find values

let ( <-= ) var = Hashtbl.replace values var

let init ?(value = Top) ?(verbose = false) ssa_graph =
  verbose_flag := verbose;
  let worklist = Queue.create () in
  Ssa.Graph.iter (fun x ((_, stmt), _) ->
      begin match !(!stmt) with
        | Move (_, Const n) -> x <-= Const n
        | Load _ -> x <-= Bottom
        | Label _ -> x <-= Bottom
        | Phi _ -> x <-= value
        | _ -> x <-= value
      end;
      if value_of x <> Top then
        Queue.add x worklist
    ) ssa_graph;
  worklist

let propagate stmt worklist =
  match !(!stmt) with
  | Move (x, e) ->
    let v = value_of x in
    begin match e with
      | Val y ->
        printf "%s := " (name_of_var x);
        x <-= value_of y;
        printf "%s\n" (string_of_value (value_of y))
      | Binop (op, Val y, Const n) ->
        printf "%s := " (name_of_var x);
        x <-= interpret' op (value_of y) (Const n)
      | Binop (op, Const n, Val y) ->
        printf "%s := " (name_of_var x);
        x <-= interpret' op (Const n) (value_of y)
      | Binop (op, Val y, Val z) ->
        printf "%s := " (name_of_var x);
        x <-= interpret' op (value_of y) (value_of z)
      | _ -> ()
    end;
    if value_of x <> v then
      Queue.add x worklist
  | Phi (x, xs) ->
    let v = value_of x in
    let vs = List.map value_of xs in
    printf "%s := " (name_of_var x);
    x <-= List.fold_left meet' (List.hd vs) (List.tl vs);
    if value_of x <> v then
      Queue.add x worklist
  | _ -> ()

let iterate worklist ssa_graph =
  while not (Queue.is_empty worklist) do
    let x = Queue.take worklist in
    let uses = Ssa.Graph.get_uses x ssa_graph in
    List.iter (fun (_, stmt) ->
        propagate stmt worklist
      ) uses
  done

let print () =
  let rows = Hashtbl.fold (fun var value rows ->
      [name_of_var var; string_of_value value] :: rows
    ) values []
  in
  print_table ~rows:(["Variable"; "Value"] :: List.sort compare rows)
