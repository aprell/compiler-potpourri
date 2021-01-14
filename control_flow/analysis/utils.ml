module M = Map.Make (struct
  type t = int
  let compare = Stdlib.compare
end)
