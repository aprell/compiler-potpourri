{
  open Parser

  exception Error of string * Lexing.position

  let lexing_error lexbuf =
    let invalid_input = String.make 1 (Lexing.lexeme_char lexbuf 0) in
    raise (Error (invalid_input, lexbuf.Lexing.lex_curr_p))
}

let whitespace = [' ' '\t' '\n']+
let letter = ['A'-'Z' 'a'-'z']
let digit = ['0'-'9']
let int = digit+
let ident = ('_' | letter) ('_' | letter | digit)*

rule read = parse
  | whitespace  { read lexbuf }
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
  | "receive"   { RECV }
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
