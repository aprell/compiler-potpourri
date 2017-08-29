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
%token INPUT OUTPUT
%token LBRACKET RBRACKET
%token EOF

%start <IR.stmt> prog

%%

prog:
  | stmt EOF                 { $1 }
  ;

stmt:
  | NAME GETS expr           { Move ($1, $3) }
  | NAME GETS mem            { Fetch ($1, $3) }
  | mem GETS scalar          { Store ($1, $3) }
  | NAME COL                 { Label $1 }
  | GOTO NAME                { Jump (Name $2) }
  | IF expr GOTO NAME        { Cond ($2, Name $4) }
  | RECV scalar              { Receive (Some $2) }
  | RECV                     { Receive None }
  | RET scalar               { Return (Some $2) }
  | RET                      { Return None }
  | INPUT scalar             { Input $2 }
  | OUTPUT scalar            { Output $2 }
  ;

expr:
  | scalar                   { Val $1 }
  | scalar binop scalar      { Binop ($2, $1, $3) }
  | scalar relop scalar      { Relop ($2, $1, $3) }
  ;

scalar:
  | INT                      { Const $1 }
  | NAME                     { Var $1 }
  ;

mem:
  | NAME LBRACKET scalar RBRACKET { Mem { base = $1; index = $3 } }

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
