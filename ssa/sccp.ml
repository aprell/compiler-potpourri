(* Sparse conditional constant propagation *)

open Three_address_code__IR
open Basic_block__Utils
open Control_flow
open Ssa__Utils

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

type op = Bin of binop | Rel of relop

let string_of_op = function
  | Bin Plus -> "+"
  | Bin Minus -> "-"
  | Bin Mul -> "*"
  | Bin Div -> "/"
  | Bin Mod -> "%"
  | Rel EQ -> "=="
  | Rel NE -> "!="
  | Rel LT -> "<"
  | Rel GT -> ">"
  | Rel LE -> "<="
  | Rel GE -> ">="

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
  | Bin Plus, Const n1, Const n2 -> Const (n1 + n2)
  | Bin Minus, Const n1, Const n2 -> Const (n1 - n2)
  | Bin Mul, Const n1, Const n2 -> Const (n1 * n2)
  | Bin Div, Const n1, Const n2 -> Const (n1 / n2)
  | Bin Mod, Const n1, Const n2 -> Const (n1 mod n2)
  | Bin Mul, Bottom, Const 0 | Bin Mul, Const 0, Bottom -> Const 0
  | Rel EQ, Const n1, Const n2 when n1 = n2 -> Const 1
  | Rel EQ, Const n1, Const n2 when n1 <> n2 -> Const 0
  | Rel NE, Const n1, Const n2 when n1 = n2 -> Const 0
  | Rel NE, Const n1, Const n2 when n1 <> n2 -> Const 1
  | Rel LT, Const n1, Const n2 when n1 < n2 -> Const 1
  | Rel LT, Const n1, Const n2 when n1 >= n2 -> Const 0
  | Rel GT, Const n1, Const n2 when n1 > n2 -> Const 1
  | Rel GT, Const n1, Const n2 when n1 <= n2 -> Const 0
  | Rel LE, Const n1, Const n2 when n1 <= n2 -> Const 1
  | Rel LE, Const n1, Const n2 when n1 > n2 -> Const 0
  | Rel GE, Const n1, Const n2 when n1 >= n2 -> Const 1
  | Rel GE, Const n1, Const n2 when n1 < n2 -> Const 0
  | _ -> Bottom

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
    (string_of_op op)
    (string_of_value v2)
    (string_of_value v3);
  v3

(* Maps variables (SSA names) to abstract values *)
let values = Hashtbl.create 10

let value_of = Hashtbl.find values

let ( <-= ) var = Hashtbl.replace values var

(* Keeps track of executed CFG edges *)
let edges = Hashtbl.create 10

let executed edge =
  Option.value (Hashtbl.find_opt edges edge) ~default:false

let execute edge =
  Hashtbl.replace edges edge true

let in_edges block =
  let open Basic_block in
  List.map (fun pred -> (pred, block)) block.pred

let out_edges block =
  let open Basic_block in
  List.map (fun succ -> (block, succ)) block.succ

let reachable block =
  in_edges block
  |> List.exists executed

let init ?(verbose = false) (graph : Cfg.t) =
  verbose_flag := verbose;
  let { Cfg.Node.block = entry; _ } = Cfg.get_entry_node graph in

  let worklist = Queue.create () in
  List.iter (fun block ->
      Queue.add (`CFG_edge (entry, block)) worklist
    ) entry.succ;

  Ssa__Def_use_chain.iter (fun x def _ ->
      match def with
      | Some (_, stmt) -> (
          match !(!stmt) with
          | Move (_, Const _) -> x <-= Top
          | Load _ -> x <-= Bottom
          | Label _ -> x <-= Bottom
          | Phi _ -> x <-= Top
          | _ -> x <-= Top
        )
      | None -> x <-= Top
    );
  worklist

let visit_stmt stmt block worklist =
  let open Basic_block in
  match !stmt with
  | Move (x, e) ->
    let v = value_of x in
    begin match e with
      | Const n ->
        printf "%s := %d\n" (name_of_var x) n;
        x <-= Const n;
      | Val y ->
        printf "%s := " (name_of_var x);
        x <-= value_of y;
        printf "%s\n" (string_of_value (value_of y))
      | Binop (op, Val y, Const n) ->
        printf "%s := " (name_of_var x);
        x <-= interpret' (Bin op) (value_of y) (Const n)
      | Binop (op, Const n, Val y) ->
        printf "%s := " (name_of_var x);
        x <-= interpret' (Bin op) (Const n) (value_of y)
      | Binop (op, Val y, Val z) ->
        printf "%s := " (name_of_var x);
        x <-= interpret' (Bin op) (value_of y) (value_of z)
      | _ -> ()
    end;
    if value_of x <> v then
      Queue.add (`Def_use_edges x) worklist
  | Jump l ->
    let target = List.hd block.succ in
    assert (entry_label target = l);
    printf "Queue %s -> %s (jump)\n" block.name target.name;
    Queue.add (`CFG_edge (block, target)) worklist
  | Cond (e, l1, l2) -> (
      let then_, else_ = match block.succ with
        | [b1; b2] when entry_label b1 = l1 && entry_label b2 = l2 -> b1, b2
        | [b1; b2] when entry_label b1 = l2 && entry_label b2 = l1 -> b2, b1
        | _ -> assert false
      in
      let v = match e with
        | Relop (op, Val x, Const n) ->
          printf "if ";
          interpret' (Rel op) (value_of x) (Const n)
        | Relop (op, Const n, Val x) ->
          printf "if ";
          interpret' (Rel op) (Const n) (value_of x)
        | Relop (op, Val x, Val y) ->
          printf "if ";
          interpret' (Rel op) (value_of x) (value_of y)
        | _ -> Bottom
      in
      match v with
      | Const 1 ->
        printf "Queue %s -> %s (then)\n" block.name then_.name;
        Queue.add (`CFG_edge (block, then_)) worklist
      | Const 0 ->
        printf "Queue %s -> %s (else)\n" block.name else_.name;
        Queue.add (`CFG_edge (block, else_)) worklist
      | Bottom ->
        printf "Queue %s -> %s (then)\n" block.name then_.name;
        printf "Queue %s -> %s (else)\n" block.name else_.name;
        Queue.add (`CFG_edge (block, then_)) worklist;
        Queue.add (`CFG_edge (block, else_)) worklist
      | _ -> assert false
    )
  | Phi (x, ([y; z] as args)) ->
    let edges = in_edges block in
    assert (List.length edges = 2);
    let v = value_of x in
    if v <> Bottom then (
      let edges = List.combine args edges in
      let e1 = List.assoc y edges in
      let e2 = List.assoc z edges in
      assert ((fst e1).number < (fst e2).number);
      if not (executed e1) then assert (executed e2);
      if not (executed e2) then assert (executed e1);
      printf "%s := " (name_of_var x);
      x <-= meet' (value_of y) (value_of z);
      if value_of x <> v then
        Queue.add (`Def_use_edges x) worklist
    )
  | _ -> ()

let visit block worklist =
  let open Basic_block in
  let phis, rest = List.partition (( ! ) >> is_phi) block.stmts in
  List.iter (fun phi -> visit_stmt phi block worklist) phis;
  if List.(length (filter executed (in_edges block)) = 1) then
    List.iter (fun non_phi -> visit_stmt non_phi block worklist) rest

let iterate worklist =
  while not (Queue.is_empty worklist) do
    match Queue.take worklist with
    | `CFG_edge ((m, n) as edge) ->
      if not (executed edge) then (
        printf "Execute %s -> %s\n" m.name n.name;
        execute edge;
        visit n worklist
      ) else (
        printf "Execute %s -> %s (already executed)\n" m.name n.name
      )
    | `Def_use_edges x ->
      let uses = Ssa__Def_use_chain.get_uses x in
      Ssa__Def_use_chain.Set.iter (fun (block, stmt) ->
          if reachable !block then
            visit_stmt !stmt !block worklist
        ) uses
  done

let print () =
  let rows = Hashtbl.fold (fun var value rows ->
      [name_of_var var; string_of_value value] :: rows
    ) values []
  in
  print_table ~rows:(["Variable"; "Value"] :: List.sort compare rows)
