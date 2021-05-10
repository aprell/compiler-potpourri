{
  open Parser

  exception Error of string

  let position lexbuf =
    let pos = lexbuf.Lexing.lex_curr_p in
    pos.pos_lnum, pos.pos_cnum - pos.pos_bol

  let lexing_error lexbuf =
    let input = Lexing.lexeme lexbuf in
    let line, col = position lexbuf in
    let msg = Printf.sprintf "%d:%d: unexpected '%s'" line col input in
    raise (Error msg)
}

let whitespace = [' ' '\t']+
let newline = ['\n']
let alpha = ['A'-'Z' 'a'-'z']
let digit = ['0'-'9']
let int = digit+
let ident = ('_' | alpha) ('_' | alpha | digit)*

rule read = parse
  | whitespace  { read lexbuf }
  | newline     { Lexing.new_line lexbuf; read lexbuf }
  | "+"         { PLUS }
  | "-"         { MINUS }
  | "*"         { MUL }
  | "/"         { DIV }
  | "%"         { MOD }
  | "=="        { EQ }
  | "!="        { NE }
  | "<"         { LT }
  | ">"         { GT }
  | "<="        { LE }
  | ">="        { GE }
  | ":="        { GETS }
  | ":"         { COL }
  | "if"        { IF }
  | "else"      { ELSE }
  | "while"     { WHILE }
  | "goto"      { GOTO }
  | "return"    { RET }
  | "("         { LPAREN }
  | ")"         { RPAREN }
  | "{"         { LBRACE }
  | "}"         { RBRACE }
  | "["         { LBRACKET }
  | "]"         { RBRACKET }
  | ","         { COMMA }
  | ident as id { NAME id }
  | int as i    { INT (int_of_string i) }
  | eof         { EOF }
  | _           { lexing_error lexbuf }
