(* Reachability analysis *)

open Three_address_code__IR
open Basic_block__Utils
open Control_flow
open Ssa__Utils

(* Abstract values *)
type value = Reachable | Unreachable

let string_of_value = function
  | Reachable -> "Reachable"
  | Unreachable -> "Unreachable"

(* Combine abstract values of two edges *)
let plus v1 v2 =
  match v1 with
  | Reachable -> v1
  | Unreachable -> v2

(* Maps basic blocks (names) to abstract values *)
let values = Hashtbl.create 10

let value_of = Hashtbl.find values

let ( <-= ) var = Hashtbl.replace values var

let last lst =
  assert (List.length lst > 0);
  List.nth lst (List.length lst - 1)

let label_of { Basic_block.name; source; } =
  match source with
  | Some { entry; _ } -> (entry, None)
  | None -> (
      match name with
      | "Entry" -> ("entry", None)
      | "Exit" -> ("exit", None)
      | _ -> assert false
    )

let interpret { Cfg.Node.block; succ; _ } =
  let v = value_of block.name in
  match block.source with
  | Some { stmts; _ } -> (
      match !(last stmts) with
      (* Reachable * v = v,
       * Unreachable * v = Unreachable *)
      | Jump l -> [(l, v)]
      | Cond (e, l1, l2) -> (
          match constant_fold e with
          | Const 0 -> [(l1, Unreachable); (l2, v)]
          | Const 1 -> [(l1, v); (l2, Unreachable)]
          | _ -> [(l1, v); (l2, v)]
        )
      | Label _ (* Falls through to exit *)
      | Return _ -> [(("exit", None), v)]
      | _ -> assert false
    )
  | None -> (
      assert (block.name = "Entry");
      assert (Cfg.NodeSet.cardinal succ = 1);
      [(label_of (Cfg.NodeSet.choose succ).block, v)]
    )

let string_of_binop = function
  | Plus -> "+"
  | Minus -> "-"
  | Mul -> "*"
  | Div -> "/"
  | Mod -> "%"

let init graph =
  let worklist = Queue.create () in
  Cfg.iter (fun ({ block; _ } as node) ->
      block.name <-= Reachable;
      if block.name <> "Entry" then
        Queue.add node worklist
    ) graph;
  worklist

let propagate worklist ({ Cfg.Node.block; pred; _ } as node) =
  assert (not (Cfg.NodeSet.is_empty pred));
  let v = value_of block.name in (
    match
      Cfg.NodeSet.elements pred
      |> List.(map (interpret >> assoc (label_of block)))
    with
    | [v] ->
      Printf.printf "%s := " block.name;
      block.name <-= v;
      Printf.printf "%s\n" (string_of_value v)
    | v :: _ as vs ->
      Printf.printf "%s := %s = "
        block.name (String.concat " + " (List.map string_of_value vs));
      block.name <-= List.fold_left plus v vs;
      Printf.printf "%s\n" (string_of_value (value_of block.name))
    | [] -> assert false
  );
  if value_of block.name <> v then
    Queue.add node worklist

let iterate worklist =
  while not (Queue.is_empty worklist) do
    let node = Queue.take worklist in
    propagate worklist node
  done

let print () =
  let rows = Hashtbl.fold (fun block value rows ->
      [block; string_of_value value] :: rows
    ) values []
  in
  print_table ~rows:(["Basic block"; "Value"] :: List.sort compare rows)
