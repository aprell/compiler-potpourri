open Three_address_code__IR
open Control_flow__Cfg
open Data_flow

module S = struct
  module Set = struct
    include Set.Make (struct
      type t = var
      let compare = Stdlib.compare
    end)

    let to_string set =
      elements set
      |> List.map (fun (Var name) -> name)
      |> String.concat ", "
  end

  let meet = Set.union

  let init { Node.block; _ } _ =
    match block.source with
    | Some { use; def; _ } ->
      { gen = Set.of_list use; kill = Set.of_list def;
        global_in = Set.empty; global_out = Set.empty }
    | None ->
      assert (block.name = "Entry" || block.name = "Exit");
      { gen = Set.empty; kill = Set.empty;
        global_in = Set.empty; global_out = Set.empty }
end

module Liveness = Data_flow_analysis (Backward_flow (S))

let fmt pp set =
  S.Set.to_string set
  |> Format.fprintf pp "{%s}"
