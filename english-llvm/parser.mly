/* Ocamlyacc parser for MicroC */

%{
open Ast

let fst (a,_,_) = a;;
let snd (_,b,_) = b;;
let trd (_,_,c) = c;;

%}

%token SEMI LPAREN RPAREN LBRACE RBRACE LSQUARE RSQUARE COMMA
%token PLUS MINUS TIMES DIVIDE ASSIGN NOT DECREMENT INCREMENT
%token EQ NEQ LT LEQ GT GEQ TRUE FALSE AND OR

%token RETURN IF ELSE FOR WHILE INT FLOAT BOOL VOID
%token INT CHAR FLOAT BOOL VOID STRING OF STRUCT TRUE FALSE LINDEX RINDEX
%token <int> NUM_LIT
%token <float> FLOAT_LIT
%token <string> STRING_LIT
%token <char> CHAR_LITERAL
%token <string> ID
%token EOF

%nonassoc NOELSE
%nonassoc ELSE
%right ASSIGN 

%left OR
%left AND
%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE
%right NOT NEG

%start program
%type <Ast.program> program

%%

program:
  decls EOF { $1 }

decls:
   /* nothing */ { [], [], [] }
 | decls vdecl { ($2 :: fst $1), snd $1, trd $1 }
 | decls fdecl { fst $1, ($2 :: snd $1), trd $1 }
 | decls sdecl { fst $1, snd $1, ($2 :: trd $1) }

fdecl:
   typ ID LPAREN formals_opt RPAREN LBRACE vdecl_list stmt_list RBRACE
     { { typ = $1;
	 fname = $2;
	 formals = $4;
	 locals = List.rev $7;
	 body = List.rev $8 } }

formals_opt:
    /* nothing */ { [] }
  | formal_list   { List.rev $1 }

formal_list:
    typ ID                   { [($1,$2)] }
  | formal_list COMMA typ ID { ($3,$4) :: $1 }

dtyp:
  INT { Int }
| STRING {String}
| FLOAT {Float}

atyp:
  dtyp dim_list { Array($1, $2) }

typ:
    dtyp { Simple($1)}
  | atyp { $1 }
  | BOOL { Bool }
  | VOID { Void}
  | CHAR {Char}
  | STRUCT ID { Struct ($2) }

dim_list:
  LSQUARE RSQUARE { 1 }
| LSQUARE RSQUARE dim_list { 1 + $3 }

index:
  LINDEX expr RINDEX { $2 }

vdecl_list:
    /* nothing */    { [] }
  | vdecl_list vdecl { $2 :: $1 }

vdecl:
    typ ID SEMI             { VarDecl($1, $2, Noexpr) }
  | typ ID ASSIGN expr SEMI { VarDecl($1, $2, $4) }

sdecl:
    STRUCT ID LBRACE vdecl_list RBRACE
      { 
        { sname = $2;
          sformals = $4;
      } 
    }

stmt_list:
    /* nothing */  { [] }
  | stmt_list stmt { $2 :: $1 }

stmt:
    expr SEMI { Expr $1 }
  | RETURN SEMI { Return Noexpr }
  | RETURN expr SEMI { Return $2 }
  | LBRACE stmt_list RBRACE { Block(List.rev $2) }
  | IF LPAREN expr RPAREN stmt %prec NOELSE { If($3, $5, Block([])) }
  | IF LPAREN expr RPAREN stmt ELSE stmt    { If($3, $5, $7) }
  | FOR LPAREN expr_opt SEMI expr SEMI expr_opt RPAREN stmt
     { For($3, $5, $7, $9) }
  | WHILE LPAREN expr RPAREN stmt { While($3, $5) }

expr_opt:
    /* nothing */ { Noexpr }
  | expr          { $1 }

id:
  ID               { Id($1) }

val_list:
    expr                { [ $1 ] }
  | expr COMMA val_list { [ $1 ] @ $3 }

simple_arr_literal:
    LSQUARE val_list RSQUARE { $2 }

expr:
    NUM_LIT          { NumLit($1) }
  | FLOAT_LIT        { FloatLit($1) }
  | STRING_LIT       { StringLit($1) }
  | CHAR_LITERAL     { CharLit($1)}
  | simple_arr_literal { ArrayLit($1)}
  | TRUE             { BoolLit(true) }
  | FALSE            { BoolLit(false) }
  | ID               { Id($1) }
  | id INCREMENT   { Pop($1, Inc) }
  | id DECREMENT   { Pop($1, Dec) }
  | expr PLUS   expr { Binop ($1, Add,   $3) }
  | expr MINUS  expr { Binop ($1, Sub,   $3) }
  | expr TIMES  expr { Binop ($1, Mult,  $3) }
  | expr DIVIDE expr { Binop ($1, Div,   $3) }
  | expr EQ     expr { Binop ($1, Equal, $3) }
  | expr NEQ    expr { Binop ($1, Neq,   $3) }
  | expr LT     expr { Binop ($1, Less,  $3) }
  | expr LEQ    expr { Binop ($1, Leq,   $3) }
  | expr GT     expr { Binop ($1, Greater, $3) }
  | expr GEQ    expr { Binop ($1, Geq,   $3) }
  | expr AND    expr { Binop ($1, And,   $3) }
  | expr OR     expr { Binop ($1, Or,    $3) }
  | MINUS expr %prec NEG { Unop(Neg, $2) }
  | NOT expr         { Unop(Not, $2) }
  | expr ASSIGN expr   { Assign($1,        $3) }
  | ID LPAREN actuals_opt RPAREN { Call($1, $3) }
  | ID LSQUARE expr RSQUARE {ArrayAccess($1, $3)}
  | ID LSQUARE expr RSQUARE ASSIGN expr { ArrayAssign($1, [$3], $6) }
  | expr index { Index($1, [$2]) }
  | LPAREN expr RPAREN { $2 }


actuals_opt:
    /* nothing */ { [] }
  | actuals_list  { List.rev $1 }

actuals_list:
    expr                    { [$1] }
  | actuals_list COMMA expr { $3 :: $1 }

