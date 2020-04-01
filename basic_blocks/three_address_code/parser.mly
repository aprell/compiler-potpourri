%{
  open IR
%}

%token <int> INT
%token <string> NAME
%token PLUS MINUS MUL DIV MOD
%token EQ NE LT GT LE GE
%token GETS
%token COL
%token IF
%token GOTO
%token RECV RET
%token LBRACKET RBRACKET
%token EOF

%left EQ NE
%left LT GT LE GE
%left PLUS MINUS
%left MUL DIV MOD

%start <IR.stmt> prog

%%

prog:
  | stmt EOF                 { $1 }
  ;

stmt:
  | NAME GETS expr           { Move (Var $1, $3) }
  | NAME GETS mem            { Load (Var $1, $3) }
  | mem GETS expr            { Store ($1, $3) }
  | NAME COL                 { Label $1 }
  | GOTO NAME                { Jump $2 }
  | IF expr GOTO NAME        { Cond ($2, $4) }
  | RECV NAME                { Receive (Var $2) }
  | RET option(expr)         { Return $2 }
  ;

expr:
  | INT                      { Const $1 }
  | NAME                     { Ref (Var $1) }
  | expr binop expr          { Binop ($2, $1, $3) }
  | expr relop expr          { Relop ($2, $1, $3) }
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
