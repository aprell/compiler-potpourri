open Three_address_code__IR
open Control_flow__Cfg
open Data_flow

module S = Set.Make (struct
  type t = var
  let compare = Stdlib.compare
end)

module Liveness = Data_flow_analysis (Backward_flow (S) (struct
  let meet = S.union

  let rec find_all_vars = function
    | Const _ -> []
    | Ref x -> [x]
    | Binop (_, e1, e2) | Relop (_, e1, e2) ->
      find_all_vars e1 @ find_all_vars e2

  let init { block = Basic_block (name, source_info); _ } _ =
    match source_info with
    | Some { stmts; _ } ->
      let rec loop gen kill = function
        | stmt :: stmts ->
          let gen', kill' = match stmt with
            | Move (x, e) ->
              (* Remove x from use (gen) *)
              let gen' = S.remove x gen in
              (* Insert x into def (kill) *)
              let kill' = S.add x kill in
              (* Insert all variables that occur in E in use (gen) *)
              let vars = S.of_list (find_all_vars e) in
              let gen' = S.union gen' vars in
              (* Remove all variables that occur in E from def (kill) *)
              let kill' = S.diff kill' vars in
              (gen', kill')
            | Load (_, _) ->
              failwith "unimplemented"
            | Store (_, _) ->
              failwith "unimplemented"
            | Cond (e, _) | Return (Some e) ->
              let vars = S.of_list (find_all_vars e) in
              let gen' = S.union gen vars in
              (gen', kill)
            | Receive x ->
              let gen' = S.remove x gen in
              let kill' = S.add x kill in
              (gen', kill')
            | _ -> (gen, kill)
          in
          loop gen' kill' stmts
        | [] -> (gen, kill)
      in
      let gen, kill = loop S.empty S.empty (List.rev stmts) in
      { gen; kill; global_in = S.empty; global_out = S.empty }
    | None ->
      assert (name = "Entry" || name = "Exit");
      { gen = S.empty; kill = S.empty; global_in = S.empty; global_out = S.empty }
end))

let fmt pp set =
  S.elements set
  |> List.map (fun (Var name) -> name)
  |> String.concat ", "
  |> Format.fprintf pp "{%s}"
