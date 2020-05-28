%{
  open IR
%}

%token <int> INT
%token <string> NAME
%token PLUS MINUS MUL DIV MOD
%token EQ NE LT GT LE GE
%token GETS
%token COL
%token IF ELSE
%token WHILE
%token GOTO
%token RECV RET
%token LPAREN RPAREN
%token LBRACE RBRACE
%token LBRACKET RBRACKET
%token COMMA
%token EOF
%token PHI "PHI"

%left EQ NE
%left LT GT LE GE
%left PLUS MINUS
%left MUL DIV MOD

%start <IR.stmt list> prog
%start <IR.stmt> line

%%

prog:
  | proc EOF
    { $1 |> lower }
  | list(stmt) EOF
    { $1 }
  ;

line:
  | stmt EOF
    { $1 }
  ;

proc:
  | name = NAME; params = params; body = block
    { Proc { name; params; body } }
  ;

params:
  | delimited(LPAREN, separated_list(COMMA, NAME), RPAREN)
    { List.map (fun x -> Var x) $1 }
  ;

block:
  | delimited(LBRACE, list(stmt), RBRACE)
    { $1 }
  ;

label:
  | NAME option(params)
    { ($1, $2) }
  ;

stmt:
  | NAME GETS expr                       { Move (Var $1, $3) }
  | NAME GETS mem                        { Load (Var $1, $3) }
  | mem GETS expr                        { Store ($1, $3) }
  | label COL                            { Label $1 }
  | GOTO label                           { Jump $2 }
  | IF expr GOTO label ELSE GOTO label   { Cond ($2, $4, $7) }
  | IF expr block                        { If ($2, $3, None) }
  | IF expr block ELSE block             { If ($2, $3, Some $5) }
  | WHILE expr block                     { Loop ($2, $3) }
  | RECV NAME                            { Receive (Var $2) }
  | RET option(expr)                     { Return $2 }
  | NAME GETS "PHI" params               { Phi (Var $1, $4) }
  ;

expr:
  | INT               { Const $1 }
  | NAME              { Ref (Var $1) }
  | expr binop expr   { Binop ($2, $1, $3) }
  | expr relop expr   { Relop ($2, $1, $3) }
  ;

mem:
  | NAME delimited(LBRACKET, expr, RBRACKET)
    { Mem { base = Addr $1; offset = $2 } }

%inline binop:
  | PLUS  { Plus }
  | MINUS { Minus }
  | MUL   { Mul }
  | DIV   { Div }
  | MOD   { Mod }
  ;

%inline relop:
  | EQ   { EQ }
  | NE   { NE }
  | LT   { LT }
  | GT   { GT }
  | LE   { LE }
  | GE   { GE }
  ;
