open MachineLang
open Lexer
open Parser
open Arg
open Printf

let parseProgram c =
    try 
		let lexbuf = Lexing.from_string c in
            parser_main lexer_main lexbuf
    with Parsing.Parse_error -> failwith "Invalid syntax"
		| Failure "lexing: empty token" -> failwith "Invalid characters"
;;

let _ =
	try
		let file = open_in Sys.argv.(1) in
		while true do
			let line = input_line file in
			let parsed = parseProgram line in
			let _ = typeProg parsed	in
			(* print_string "Program Type\t\t==> Checked\n"; *)
			let result3 = bigEval parsed in
			(* print_string "Program Evaluated\t==> " ; *)
			let () = print_res result3; in
				flush stdout
		done
	with End_of_file -> exit 0