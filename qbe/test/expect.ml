let input = ref None
let optimize = ref false

let parse_args () =
  let prog = Sys.argv.(0) in
  let usage = Printf.sprintf "Usage: %s [-opt] filename" prog in
  let options = ["-opt", Arg.Set optimize, "Enable optimization"] in
  Arg.parse options (fun filename -> input := Some filename) usage

let () =
  parse_args ();
  match !input with
  | Some filename ->
    let optimize = !optimize in
    Qbe.emit filename ~optimize
  | None -> ()
