open Utils

(*$< IR *)
(*$inject failwith "TODO" *)
(*$>*)
let parse_line line =
  let lexbuf = Lexing.from_string line in
  Parser.prog Lexer.read lexbuf

let parse_file name =
  read_file name |> List.map parse_line
