/* MachineLang parser */
%{
	open MachineLang
%}

%token <string> WORD
%token <string> LANG
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
%nonassoc LANG

/* highest precedence */

%start parser_main
%type <MachineLang.machTerm> parser_main
%type <MachineLang.machType> type_spec
%%
type_spec: FUNCTYPE type_spec type_spec		{ MachFunc ($2, $3) }
	|	WORDTYPE							{ MachWord }
	|	LANGTYPE							{ MachLang }
	|	INTTYPE								{ MachInt }
;

parser_main: expr EOL { $1 }
;
expr: 	WORD						{ MtWord $1 }
	|	LANG						{ MtLang $1 }
	|	INT							{ MtNum $1 }
	|	PREFIX expr expr			{ MtOpp ($2,$3,MachPrefix) }
	|	UNION expr expr				{ MtOpp ($2,$3,MachUnion) }
	|	INSEC expr expr				{ MtOpp ($2,$3,MachInsec) }