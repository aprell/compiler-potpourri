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
    { $1 }
  | list(stmt) EOF
    { $1 }
  ;

line:
  | stmt EOF
    { $1 }
  ;

proc:
  | name = NAME; params = params; body = block
    { `Proc (name, params, body) |> lower }
  ;

params:
  | delimited(LPAREN, separated_list(COMMA, NAME), RPAREN)
    { List.map (fun x -> Var x) $1 }
  ;

block:
  | delimited(LBRACE, flatten(list(hl_stmt)), RBRACE)
    { $1 }
  ;

label:
  | NAME option(params)
    { ($1, $2) }
  ;

hl_stmt:
  | NAME GETS mem                        { `Load (Var $1, $3) |> lower }
  | mem GETS expr                        { `Store ($1, $3)    |> lower }
  | IF expr block                        { `If ($2, $3, [])   |> lower }
  | IF expr block ELSE block             { `If ($2, $3, $5)   |> lower }
  | WHILE expr block                     { `While ($2, $3)    |> lower }
  | stmt                                 { [$1] }
  ;

stmt:
  | NAME GETS expr                       { Move (Var $1, $3) }
  | label COL                            { Label $1 }
  | GOTO label                           { Jump $2 }
  | IF expr GOTO label ELSE GOTO label   { Cond ($2, $4, $7) }
  | RECV NAME                            { Receive (Var $2) }
  | RET option(expr)                     { Return $2 }
  | NAME GETS "PHI" params               { Phi (Var $1, $4) }
  ;

expr:
  | INT               { Const $1 }
  | NAME              { Val (Var $1) }
  | expr binop expr   { Binop ($2, $1, $3) }
  | expr relop expr   { Relop ($2, $1, $3) }
  ;

mem:
  | base = NAME; index = delimited(LBRACKET, expr, RBRACKET)
    { `Addr (Var base, index) }
  ;

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
