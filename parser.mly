(* machineLang parser *)
%{
	open machineLang
%}

%token <string> WORD
%token <int> INT
%token PREFIX
%token UNION
%token INSEC
%token CURLLEFT
%token CURLRIGHT
%token EOF

/* lowest precedence */

%nonassoc PREFIX UNION INSEC

/* highest precedence */

%start parser_main
%type <machineLang.term> parser_main
%