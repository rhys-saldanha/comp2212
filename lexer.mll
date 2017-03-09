(* machineLang lexer *)
(
	open Parser			(* the type token will be defined in parser.mly *)
)

rule lexer_main = parse
		[' ' '\t' '\n']		{ lexer_main lexbuf }	(* skip blanks *)
	|	[