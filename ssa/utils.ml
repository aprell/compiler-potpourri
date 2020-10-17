open Three_address_code__IR

let ( >> ) f g x = g (f x)

let ( !! ) = ( ! ) >> ( ! )

let is_phi_function = function
  | Phi _ -> true
  | _ -> false
