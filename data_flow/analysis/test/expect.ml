open Three_address_code__IR
open Basic_block__Utils
open Control_flow__Graphs
open Data_flow
open Liveness

let string_of_vars vars =
  List.map (fun (Var x) -> x) vars
  |> String.concat ", "

let print_use_def sets =
  let column_widths = [5; 20; 20] in
  let rows = List.rev (Array.fold_left (fun rows { gen; kill; _ } ->
      [string_of_vars (S.elements gen); string_of_vars (S.elements kill)] :: rows
    ) [] sets)
  in
  let rows = List.map2 (fun x y -> x :: y)
      ("Entry" :: ("B" ^^ (1 -- (Array.length sets - 2))) @ ["Exit"])
      rows
  in
  print_table
    ~column_widths
    ~rows:([""; "use"; "def"] :: rows)

let print_in_out sets =
  let column_widths = [5; 20; 20] in
  let rows = List.rev (Array.fold_left (fun rows { global_in; global_out; _ } ->
      [string_of_vars (S.elements global_in); string_of_vars (S.elements global_out)] :: rows
    ) [] sets)
  in
  let rows = List.map2 (fun x y -> x :: y)
      ("Entry" :: ("B" ^^ (1 -- (Array.length sets - 2))) @ ["Exit"])
      rows
  in
  print_table
    ~column_widths
    ~rows:([""; "IN"; "OUT"] :: rows)

let () =
  match Sys.argv with
  | [| _; filename |] ->
    let graph = graph_of_input filename in
    let sets = Liveness.compute graph in
    print_use_def sets;
    print_newline ();
    print_in_out sets
  | _ ->
    failwith "Input file required"
