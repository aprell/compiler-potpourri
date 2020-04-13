open Control_flow__Cfg
open Data_flow

module S = Set.Make (struct
  type t = Three_address_code__IR.name
  let compare = Stdlib.compare
end)

module Dominators = Data_flow_analysis (Forward_flow (S) (struct
  open Node

  let meet = S.inter

  let all_basic_blocks =
    Array.fold_left (fun set { block = Basic_block (name, _); _ } ->
        S.add name set
      ) S.empty

  let init { block = Basic_block (name, _); _ } graph =
    { gen = S.singleton name;
      kill = S.empty;
      global_in = S.empty;
      global_out = match name with
        | "Entry" -> S.empty
        | _ -> all_basic_blocks graph }
end))

let fmt pp set =
  S.elements set
  |> String.concat ", "
  |> Format.fprintf pp "{%s}"
