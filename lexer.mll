(* MachineLang lexer *)
{
	open Parser			(* the type token will be defined in parser.mly *)
	open Str
	exception End_of_file
}

rule lexer_main = parse
	  [' ' '\t' '(' ')']					{ lexer_main lexbuf }	(* skip blanks *)
	| ['\n']								{ EOL }
	| ['0'-'9']+ as lxm 					{ INT (int_of_string lxm) }
	| '{'									{ LBEGIN }
	| '}'									{ LEND }
	| ','									{ COMMA }
	| ['"']['a'-'z']+['"'] as lxm 			{ WORD (global_replace (regexp "\"") "" lxm) }
	| ['\'']['a'-'z']+['\''] as lxm			{ VAR (global_replace (regexp "'") "" lxm) }
	| "star"								{ STAR }
	| "gen"									{ LANGGEN }
	| "assign"								{ ASSIGN }
	| "prefix"								{ PREFIX }
	| "union"								{ UNION }
	| "insec"								{ INSEC }
	| "concat"								{ CONCAT }
	| "END"									{ raise End_of_file }
    | eof      								{ EOL }