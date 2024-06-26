%{
  open IR

  let prev = ref []

  let ( @? ) l1 l2 =
    match List.hd l1, l2 with
    (* Drop copy of self *)
    | Move (x, Val y), _ when x = y -> l2
    | _ -> l1 @ l2

  let peephole stmts =
    let rec loop acc = function
      | stmt :: stmts -> (
          match !prev, stmt with

          (* Propagate constant to next statement *)
          | [Move (x, Const n)], Move (y, Val z) when x = z ->
            prev := [Move (y, Const n)];
            loop (!prev @? acc) stmts
          | [Move (x, Const n)], Move (y, Binop (op, Val z, e)) when x = z ->
            prev := [Move (y, constant_fold (Binop (op, Const n, e)))];
            loop (!prev @? acc) stmts
          | [Move (x, Const n)], Move (y, Binop (op, e, Val z)) when x = z ->
            prev := [Move (y, constant_fold (Binop (op, e, Const n)))];
            loop (!prev @? acc) stmts

          (* Propagate copy to next statement *)
          | [Move (x, Val x')], Move (y, Val z) when x = z ->
            prev := [Move (y, Val x')];
            loop (!prev @? acc) stmts
          | [Move (x, Val x')], Move (y, Binop (op, Val z, e)) when x = z ->
            prev := [Move (y, constant_fold (Binop (op, Val x', e)))];
            loop (!prev @? acc) stmts
          | [Move (x, Val x')], Move (y, Binop (op, e, Val z)) when x = z ->
            prev := [Move (y, constant_fold (Binop (op, e, Val x')))];
            loop (!prev @? acc) stmts

          | _ ->
            prev := [stmt];
            loop (!prev @? acc) stmts
        )
      | [] -> List.rev acc
    in
    loop [] stmts
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
%token RET
%token LPAREN RPAREN
%token LBRACE RBRACE
%token LBRACKET RBRACKET
%token COMMA
%token EOF
%token PHI "PHI"
%token FUN "fun"

%left EQ NE
%left LT GT LE GE
%left PLUS MINUS
%left MUL DIV MOD

%type <IR.decl option * IR.stmt list> prog
%type <IR.decl * IR.stmt list> func
%type <IR.stmt> stmt
%type <IR.stmt list> block hir_stmt
%type <IR.var list> params
%type <(IR.var * IR.Type.t) list> typed_params
%type <IR.name * IR.var list option> label
%type <IR.expr> expr

%start prog

%%

prog:
  | func EOF
    { Some (fst $1), snd $1 }
  | stmt* EOF
    { None, $1 }
  ;

func:
  | "fun" name = NAME; params = typed_params; return_type = type_annot?; body = block
    { let params, param_types = List.split params in
      let return_type = Option.value return_type ~default:IR.Type.Void in
        FunDecl { name; typesig = (return_type, param_types) },
        `Function (name, params, body) |> lower
    }
  ;

params:
  | delimited(LPAREN, separated_list(COMMA, NAME), RPAREN)
    { List.map (fun x -> Var x) $1 }
  ;

typed_params:
  | delimited(LPAREN, separated_list(COMMA, typed_name), RPAREN)
    { List.map (fun (x, y) -> Var x, y) $1 }
  ;

block:
  | block_entry delimited(LBRACE, flatten(hir_stmt*), RBRACE)
    { $2 }
  ;

typed_name:
  | NAME type_annot
    { $1, $2 }
  ;

type_annot:
  | preceded(COL, type_)
    { $1 }
  ;

type_:
  | NAME pair(LBRACKET, RBRACKET)?
    { match $1, $2 with
      | "int",  Some _ -> IR.Type.Ptr IR.Type.Int
      | "int",  None   -> IR.Type.Int
      | "void", Some _ -> IR.Type.Ptr IR.Type.Void
      | "void", None   -> IR.Type.Void
      | name, _ -> failwith ("Unknown type name " ^ name)
    }
  ;

(* Reset peephole to avoid optimizing across basic block boundaries *)
block_entry: { prev := [] }

label:
  | NAME params?
    { $1, $2 }
  ;

hir_stmt:
  | NAME GETS mem               { `Load (Var $1, $3) |> lower }
  | mem GETS expr               { `Store ($1, $3)    |> lower }
  | IF expr block               { `If ($2, $3, [])   |> lower }
  | IF expr block ELSE block    { `If ($2, $3, $5)   |> lower }
  | WHILE expr block            { `While ($2, $3)    |> lower }
  | stmt                        { normalize $1       |> peephole }
  ;

stmt:
  | NAME GETS expr                        { Move (Var $1, $3) }
  | label COL                             { Label $1 }
  | GOTO label                            { Jump $2 }
  | IF expr GOTO label ELSE GOTO label    { Cond ($2, $4, $7) }
  | RET expr?                             { Return $2 }
  | NAME GETS "PHI" params                { Phi (Var $1, $4) }
  ;

expr:
  | INT                { Const $1 }
  | NAME               { Val (Var $1) }
  | MINUS expr         { Binop (Minus, Const 0, $2) |> constant_fold }
  | expr binop expr    { Binop ($2, $1, $3)         |> constant_fold }
  | expr relop expr    { Relop ($2, $1, $3)         |> constant_fold }
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
  | EQ { EQ }
  | NE { NE }
  | LT { LT }
  | GT { GT }
  | LE { LE }
  | GE { GE }
  ;
