open Three_address_code

let input = ref None
let unparse = ref false

let parse_args () =
  let prog = Sys.argv.(0) in
  let usage = Printf.sprintf "Usage: %s [-unparse] filename" prog in
  let options = ["-unparse", Arg.Set unparse, "Unparse input"] in
  Arg.parse options (fun filename -> input := Some filename) usage

let () =
  parse_args ();
  match !input with
  | Some filename ->
    let _, stmts = Parse.parse_file filename in
    if !unparse then IR.dump stmts
  | None -> ()
