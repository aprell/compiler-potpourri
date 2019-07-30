module IntSet = Set.Make (struct
  type t = int
  let compare = Stdlib.compare
end)

module Nodes = IntSet
