(* Reachability analysis *)

open Three_address_code__IR
open Basic_block__Utils
open Control_flow
open Ssa__Utils

let verbose_flag = ref false

let printf fmt =
  if !verbose_flag then Printf.fprintf stdout fmt
  else Printf.ifprintf stdout fmt

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

let interpret block =
  let open Basic_block in
  let v = value_of block.name in
  match last_stmt block with
  (* Reachable * v = v,
   * Unreachable * v = Unreachable *)
  | Some { contents = Jump l } -> [(l, v)]
  | Some { contents = Cond (e, l1, l2) } -> (
      match constant_fold e with
      | Const 0 -> [(l1, Unreachable); (l2, v)]
      | Const 1 -> [(l1, v); (l2, Unreachable)]
      | _ -> [(l1, v); (l2, v)]
    )
  | Some { contents = Label _ } (* Falls through to exit *)
  | Some { contents = Return _ } -> [(("Exit", None), v)]
  | Some _ -> assert false
  | None ->
    assert (block.name = "Entry");
    assert (List.length block.succ = 1);
    [(entry_label (List.hd block.succ), v)]

let init ?(verbose = false) graph =
  verbose_flag := verbose;
  let worklist = Queue.create () in
  Cfg.iter (fun { block; _ } ->
      if block.name = "Entry" then (
        block.name <-= Reachable
      ) else (
        block.name <-= Unreachable;
        Queue.add block worklist
      )
    ) graph;
  worklist

let propagate block worklist =
  let open Basic_block in
  let v = value_of block.name in (
    match List.(map (interpret >> assoc (entry_label block))) block.pred with
    | [v] ->
      printf "%s := " block.name;
      block.name <-= v;
      printf "%s\n" (string_of_value v)
    | v :: _ as vs ->
      printf "%s := %s = "
        block.name (String.concat " + " (List.map string_of_value vs));
      block.name <-= List.fold_left plus v vs;
      printf "%s\n" (string_of_value (value_of block.name))
    | [] -> assert false
  );
  if value_of block.name <> v then
    Queue.add block worklist

let iterate worklist =
  while not (Queue.is_empty worklist) do
    let block = Queue.take worklist in
    propagate block worklist
  done

let print () =
  let rows = Hashtbl.fold (fun block value rows ->
      [block; string_of_value value] :: rows
    ) values []
  in
  print_table ~rows:(["Basic block"; "Value"] :: List.sort compare rows)
