(* MachineLang lexer *)
{
	open Parser			(* the type token will be defined in parser.mly *)
	open Str
	exception Eof
}

rule lexer_main = parse
	  [' ' '\t' '\n']						{ lexer_main lexbuf }	(* skip blanks *)
	| [';'] 								{ EOL }
	| ['0'-'9']+ as lxm 					{ INT (int_of_string lxm) }
(*	| (['{'][' ' '\t' 'n']* )(['"']['a'-'z']*['"'][' ' '\t']* )([','][' ' '\t' 'n']*['"']['a'-'z']*['"'])*('}') as lxm { LANG lxm } *)
	| '{'									{ LBEGIN }
	| '}'									{ LEND }
	| ','									{ COMMA }
	| ['"']['a'-'z']+['"'] as lxm 			{ WORD (global_replace (regexp "\"") "" lxm) }
	| "prefix"								{ PREFIX }
	| "union"								{ UNION }
	| "insec"								{ INSEC }
	| "concat"								{ CONCAT }
	| "exit"								{ raise Eof }
    | eof      								{ raise Eof }
	