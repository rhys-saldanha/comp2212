/* File parser.mly */
%{
    open Toy
%}
%token <int> INT
%token <string> IDENT
%token LAMBDA
%token PLUS MINUS
%token LPAREN RPAREN
%token LET IN EQUALS 
%token IF THEN ELSE 
%token BTYPE ITYPE FUNTYPE
%token TRUE FALSE
%token LESSTHAN
%token COLON
%token EOF

/* lowest precedence */
%nonassoc IF THEN ELSE LET IN
%left FUNTYPE          
%nonassoc LESSTHAN
%left PLUS MINUS
/* highest precedence */

%start parser_main             /* the entry point */
%type <Toy.toyTerm> parser_main
%type <Toy.toyType> type_spec
%%
type_spec: ITYPE { ToyInt }
    | BTYPE     { ToyBool }
    | type_spec FUNTYPE type_spec { ToyFun ($1,$3) }
    | LPAREN type_spec RPAREN {$2}
;

parser_main: expr EOF { $1 }
;

expr: INT                      { TmNum $1 }
 | FALSE                       { TmBool false }
 | TRUE                        { TmBool true }
 | IDENT                       { TmVar $1 }
 | LET LPAREN IDENT COLON type_spec RPAREN EQUALS expr IN expr { TmLet ($3, $5, $8, $10) }
 | expr LPAREN expr RPAREN     { TmApp ($1, $3) }
 | expr PLUS expr              { TmPlus ($1, $3) }
 | expr MINUS expr             { TmMinus ($1, $3) }
 | expr LESSTHAN expr          { TmLessThan ($1, $3) }
 | IF LPAREN expr RPAREN THEN expr ELSE expr { TmIf ($3, $6, $8) }
 | LAMBDA LPAREN IDENT COLON type_spec RPAREN expr {TmAbs ($3, $5, $7) }
 | LPAREN expr RPAREN          { $2 }
;
