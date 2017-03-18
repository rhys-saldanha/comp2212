open MachineLang
open Lexer
open Parser
open Arg
open Printf

let parseProgram c =
    try 
		let lexbuf = Lexing.from_string c in
            parser_main lexer_main lexbuf
    with Parsing.Parse_error -> failwith "Parse failure!"
;;

(*try while true do
		let arg = ref stdin in
		let setProg p = arg := open_in p in
		let usage = "./main PROGRAM_FILE" in
			parse [] setProg usage ; 
		let parsedProg = parseProgram !arg in
		let () = print_string "Program Parsed" ; print_newline() in
		let _ = typeProg parsedProg in
		let () = print_string "Program Type Checked" ; print_newline() in
		let result3 = bigEval parsedProg in
		let () = print_string "Program Evaluated using big step semantics to ==> " ;  print_res result3 ; print_newline() in
			flush stdout
	done
with Lexer.Eof -> exit 0*)

let _ =
	try
		let file = open_in Sys.argv.(1) in
		while true do
			let line = input_line file in
			let parsed = parseProgram line in
			let _ = typeProg parsed	in
			print_string "Program Type\t\t==> Checked\n";
			let result3 = bigEval parsed in
			let () = print_string "Program Evaluated\t==> " ;  print_res result3 in
				flush stdout
		done
	with End_of_file -> exit 0