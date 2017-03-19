/* MachineLang parser */
%{
	open MachineLang
%}

%token <string> WORD VAR FILE
%token LBEGIN COMMA LEND ASSIGN
%token <int> INT

%token PREFIX UNION INSEC CONCAT STAR LANGGEN PRINT OPEN

%token WORDTYPE LANGTYPE INTTYPE
%token FUNCTYPE
%token EOL

/* lowest precedence */

%nonassoc PREFIX UNION INSEC CONCAT STAR PRINT OPEN
%nonassoc WORD VAR FILE
%nonassoc ASSIGN

/* highest precedence */

%start parser_main
%type <MachineLang.machTerm> parser_main
%%

parser_main: expr EOL { $1 }
;
expr: WORD						{ MtWord $1 }
	| langexpr					{ $1 }
	| INT						{ MtNum $1 }
	| PREFIX expr expr expr		{ MtOpp ($2,$3,$4,MachPrefix) }
	| UNION expr expr expr		{ MtOpp ($2,$3,$4,MachUnion) }
	| INSEC expr expr expr		{ MtOpp ($2,$3,$4,MachInsec) }
	| CONCAT expr expr expr		{ MtOpp ($2,$3,$4,MachConcat) }
	| STAR expr expr			{ MtOpp ($2,MtLang [],$3,MachStar) }
	| LANGGEN expr expr			{ MtOpp ($2,MtLang [],$3,MachGen) }
	| ASSIGN expr expr			{ MtAsn ($2, $3) }
	| VAR						{ MtVar $1 }
	| PRINT expr				{ MtPrint $2 }
	| OPEN FILE					{ MtOpen (MtFile $2) }
;
langexpr: LBEGIN LEND			{ MtLang [] }
	| LBEGIN stringexpr LEND	{ MtLang $2 }
;
stringexpr: WORD				{ [$1] }
	| WORD COMMA stringexpr		{ $1::$3 }
;