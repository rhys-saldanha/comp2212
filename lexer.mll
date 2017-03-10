(* MachineLang lexer *)
{
	open Parser			(* the type token will be defined in parser.mly *)
}

rule lexer_main = parse
		[' ' '\t']						{ lexer_main lexbuf }	(* skip blanks *)
	|	['\n' ]  { EOL }
	|	['0'-'9']+ as lxm 					{ INT (int_of_string lxm) }
	|	('{')(['"']['a'-'z']*['"'])([',']['"']['a'-'z']*['"'])*('}') as lxm { LANG lxm }
	|	['"']['a'-'z']+['"'] as lxm 		{ WORD lxm }
	|	"prefix"							{ PREFIX }
	|	"union"								{ UNION }
	|	"insec"								{ INSEC }
	|	eof									{ EOF }
	