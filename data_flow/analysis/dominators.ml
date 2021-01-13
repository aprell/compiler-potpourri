open Three_address_code__IR
open Control_flow__Cfg
open Data_flow

module S = struct
  module Set = struct
    include Set.Make (struct
      type t = name
      let compare = Stdlib.compare
    end)

    let to_string set =
      elements set
      |> String.concat ", "
  end

  let meet = Set.inter

  let basic_block_names graph =
    get_nodes graph
    |> List.map (fun { Node.block; _ } -> block.name)
    |> Set.of_list

  let init { Node.block; _ } graph =
    { gen = Set.singleton block.name;
      kill = Set.empty;
      global_in = Set.empty;
      global_out = match block.name with
        | "Entry" -> Set.empty
        | _ -> basic_block_names graph }
end

module Dominators = Data_flow_analysis (Forward_flow (S))

let fmt pp set =
  S.Set.to_string set
  |> Format.fprintf pp "{%s}"
