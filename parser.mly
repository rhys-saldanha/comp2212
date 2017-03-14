/* MachineLang parser */
%{
	open MachineLang
%}

%token <string> WORD
%token LBEGIN COMMA LEND
%token <int> INT

%token PREFIX UNION INSEC CONCAT

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
	| PREFIX expr expr expr		{ MtOpp ($2,$3,$4,MachPrefix) }
	| UNION expr expr expr		{ MtOpp ($2,$3,$4,MachUnion) }
	| INSEC expr expr expr		{ MtOpp ($2,$3,$4,MachInsec) }
	| CONCAT expr expr expr		{ MtOpp ($2,$3,$4,MachConcat) }

;
langexpr: LBEGIN LEND			{ MtLang [] }
	| LBEGIN stringexpr LEND	{ MtLang $2 }
;
stringexpr: WORD				{ [$1] }
	| WORD COMMA stringexpr		{ $1::$3 }
;