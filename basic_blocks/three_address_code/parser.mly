%{
  open IR

  let prev = ref []

  let peephole stmt =
    match !prev, stmt with

    (* Propagate constant to next statement *)
    | [Move (x, Const n)], Move (y, Val z) when x = z ->
      prev := [Move (y, Const n)]; !prev
    | [Move (x, Const n)], Move (y, Binop (op, Val z, e)) when x = z ->
      prev := [Move (y, constant_fold (Binop (op, Const n, e)))]; !prev
    | [Move (x, Const n)], Move (y, Binop (op, e, Val z)) when x = z ->
      prev := [Move (y, constant_fold (Binop (op, e, Const n)))]; !prev

    (* Propagate copy to next statement *)
    | [Move (x, Val x')], Move (y, Val z) when x = z ->
      if x' <> y then (prev := [Move (y, Val x')]; !prev) else []
    | [Move (x, Val x')], Move (y, Binop (op, Val z, e)) when x = z ->
      prev := [Move (y, constant_fold (Binop (op, Val x', e)))]; !prev
    | [Move (x, Val x')], Move (y, Binop (op, e, Val z)) when x = z ->
      prev := [Move (y, constant_fold (Binop (op, e, Val x')))]; !prev

    (* Subtract equal inputs *)
    | _, Move (x, Binop (Minus, Val y, Val z)) when y = z ->
      prev := [Move (x, Const 0)]; !prev

    (* Drop copy of self *)
    | _, Move (x, Val y) when x = y ->
      []

    | _ -> prev := [stmt]; !prev
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
  | block_entry delimited(LBRACE, flatten(list(hir_stmt)), RBRACE)
    { $2 }
  ;

(* Reset peephole to avoid optimizing across basic block boundaries *)
block_entry: { prev := [] }

label:
  | NAME option(params)
    { ($1, $2) }
  ;

hir_stmt:
  | NAME GETS mem                        { `Load (Var $1, $3) |> lower }
  | mem GETS expr                        { `Store ($1, $3)    |> lower }
  | IF expr block                        { `If ($2, $3, [])   |> lower }
  | IF expr block ELSE block             { `If ($2, $3, $5)   |> lower }
  | WHILE expr block                     { `While ($2, $3)    |> lower }
  | stmt                                 { peephole $1 }
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
  | expr binop expr   { Binop ($2, $1, $3) |> constant_fold }
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
