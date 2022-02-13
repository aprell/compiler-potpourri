let input = ref None
let optimize = ref false

let parse_args () =
  let prog = Sys.argv.(0) in
  let usage = Printf.sprintf "Usage: %s [-opt] name_of_test" prog in
  let options = ["-opt", Arg.Set optimize, "Enable optimization"] in
  Arg.parse options (fun test -> input := Some test) usage

let () =
  parse_args ();
  match !input with
  | Some test ->
    let optimize = !optimize in
    Qbe.Test.emit test ~optimize
  | None -> ()
