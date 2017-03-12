(* MachineLang lexer *)
{
	open Parser			(* the type token will be defined in parser.mly *)
	exception Eof
}

rule lexer_main = parse
		[' ' '\t' '\n']						{ lexer_main lexbuf }	(* skip blanks *)
	|	[';' ] 								{ EOL }
	|	['0'-'9']+ as lxm 					{ INT (int_of_string lxm) }
	|	(['{'][' ' '\t' 'n']*)(['"']['a'-'z']*['"'][' ' '\t']*)([','][' ' '\t' 'n']*['"']['a'-'z']*['"'])*('}') as lxm { LANG lxm }
	|	['"']['a'-'z']+['"'] as lxm 		{ WORD lxm }
	|	"prefix"							{ PREFIX }
	|	"union"								{ UNION }
	|	"insec"								{ INSEC }
    |	eof      							{ raise Eof }
	