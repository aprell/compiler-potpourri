open Three_address_code__IR
open Control_flow__Cfg
open Data_flow

module S = Set.Make (struct
  type t = var
  let compare = Stdlib.compare
end)

module Liveness = Data_flow_analysis (Backward_flow (S) (struct
  open Node

  let meet = S.union

  let init { block; _ } _ =
    match block.source with
    | Some { use; def; _ } ->
      let gen = S.of_list use in
      let kill = S.of_list def in
      { gen; kill; global_in = S.empty; global_out = S.empty }
    | None ->
      assert (block.name = "Entry" || block.name = "Exit");
      { gen = S.empty; kill = S.empty; global_in = S.empty; global_out = S.empty }
end))

let fmt pp set =
  S.elements set
  |> List.map (fun (Var name) -> name)
  |> String.concat ", "
  |> Format.fprintf pp "{%s}"
