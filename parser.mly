/* MachineLang parser */
%{
	open MachineLang
	open Str
%}

%token <string> WORD
%token LBEGIN COMMA LEND
%token <int> INT

%token PREFIX
%token UNION
%token INSEC

%token WORDTYPE LANGTYPE INTTYPE
%token FUNCTYPE
%token EOL

/* lowest precedence */

%nonassoc PREFIX UNION INSEC
%nonassoc WORD

/* highest precedence */

%start parser_main
%type <MachineLang.machTerm> parser_main
%type <MachineLang.machType> type_spec
%%
type_spec: FUNCTYPE type_spec type_spec		{ MachFunc ($2, $3) }
	| WORDTYPE								{ MachWord }
	| LANGTYPE								{ MachLang }
	| INTTYPE								{ MachInt }
;

parser_main: expr EOL { $1 }
;
expr: WORD						{ MtWord $1 }
	| langexpr					{ $1 }
	| INT						{ MtNum $1 }
	| PREFIX expr expr			{ MtOpp ($2,$3,MachPrefix) }
	| UNION expr expr			{ MtOpp ($2,$3,MachUnion) }
	| INSEC expr expr			{ MtOpp ($2,$3,MachInsec) }
;
langexpr: LBEGIN LEND			{ MtLang [] }
	| LBEGIN stringexpr LEND	{ MtLang $2 }
;
stringexpr: WORD				{ [(global_replace (regexp "\"") "" $1)] }
	| WORD COMMA stringexpr		{ (global_replace (regexp "\"") "" $1)::$3 }
;