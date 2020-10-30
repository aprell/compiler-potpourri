open Three_address_code__IR

let value_numbers = Hashtbl.create 10

let available_exprs = Hashtbl.create 10

let gen_number init =
  let count = ref init in
  fun () ->
    let c = !count in
    incr count; c

let next_value_number = gen_number 1

let rec value_number = function
  | Const n -> (
      let s = string_of_int n in
      match Hashtbl.find_opt value_numbers s with
      | Some n -> n
      | None ->
        let n = next_value_number () in
        Hashtbl.add value_numbers s n; n
    )
  | Val (Var x) -> (
      match Hashtbl.find_opt value_numbers x with
      | Some n -> n
      | None ->
        let n = next_value_number () in
        Hashtbl.add value_numbers x n; n
    )
  | (Binop (_, e1, e2) as e)
  | (Relop (_, e1, e2) as e) -> (
      let n1 = value_number e1 in
      let n2 = value_number e2 in
      let s = string_of_expr (
          match e with
          | Binop (op, _, _) when op = Plus || op = Mul ->
            if n1 < n2 then Binop (op, Const n1, Const n2)
            else Binop (op, Const n2, Const n1)
          | Binop (op, _, _) ->
            Binop (op, Const n1, Const n2)
          | Relop (op, _, _) when op = EQ || op = LE ->
            if n1 < n2 then Relop (op, Const n1, Const n2)
            else Relop (op, Const n2, Const n1)
          | Relop (op, _, _) ->
            Relop (op, Const n1, Const n2)
          | _ -> assert false
        )
      in
      match Hashtbl.find_opt value_numbers s with
      | Some n -> n
      | None ->
        let n = next_value_number () in
        Hashtbl.add value_numbers s n; n
    )

(*
let visit stmt =
  match !stmt with
  | Move (x, e) ->
    let n = value_number e in
    let _ = match e with
      | Binop _ | Relop _ when Hashtbl.mem available_exprs n ->
        let y = Hashtbl.find available_exprs n in
        stmt := Move (x, Val y)
      | _ -> Hashtbl.add available_exprs n x
    in
    Hashtbl.add value_numbers (name_of_var x) n
  | Cond (e, l1, l2) -> (
    let n = value_number e in
    match Hashtbl.find_opt available_exprs n with
    | Some y -> stmt := Cond (Val y, l1, l2)
    | None -> ()
  )
  | Return (Some e) -> (
    let n = value_number e in
    match Hashtbl.find_opt available_exprs n with
    | Some y -> stmt := Return (Some (Val y))
    | None -> ()
  )
  | _ -> ()
*)
