(* MachineLang lexer *)
{
	open Parser			(* the type token will be defined in parser.mly *)
	open Str
	exception End_of_file
}

rule lexer_main = parse
	  [' ' '\t']					{ lexer_main lexbuf }	(* skip blanks *)
	| ['\n']								{ EOL }
    | eof      								{ EOL }
	| ['0'-'9']+ as lxm 					{ INT (int_of_string lxm) }
	| ['"']['a'-'z']+['"'] as lxm 			{ WORD (global_replace (regexp "\"") "" lxm) }
	| '{'									{ LBEGIN }
	| '}'									{ LEND }
	| '('									{ PBEGIN }
	| ')'									{ PEND }
	| ','									{ COMMA }
	| "open"								{ OPEN }
	| "readline"							{ READ }
	| "INT"									{ INTTYPE }
	| "WORD"								{ WORDTYPE }
	| "LANG"								{ LANGTYPE }
	| "print"								{ PRINT }
	| "star"								{ STAR }
	| "gen"									{ LANGGEN }
	| "assign"								{ ASSIGN }
	| "prefix"								{ PREFIX }
	| "union"								{ UNION }
	| "insec"								{ INSEC }
	| "concat"								{ CONCAT }
	| "END"									{ raise End_of_file }
	(* | ['\''] _+ ['\''] as lxm				{ FILE (global_replace (regexp "'") "" lxm) } *)
	| ['a'-'z' '0'-'9' 'A'-'Z']+ as lxm		{ VAR lxm }