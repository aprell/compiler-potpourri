open Three_address_code__IR
open Control_flow__Cfg
open Data_flow

module S = struct
  module Set = struct
    include Basic_block.Liveness.Set

    let to_string set =
      elements set
      |> List.map (fun (Var name) -> name)
      |> String.concat ", "
  end

  let meet = Set.union

  let init { Node.block; _ } _ =
    let use, def = Basic_block.Liveness.compute block in
    { gen = use; kill = def;
      global_in = Set.empty; global_out = Set.empty; }
end

module Liveness = Data_flow_analysis (Backward_flow (S))

let fmt pp set =
  S.Set.to_string set
  |> Format.fprintf pp "{%s}"
