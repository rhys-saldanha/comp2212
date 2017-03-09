exception LookupError ;;
exception TypeError ;;
exception UnboundVariableError;;
exception Terminated ;;
exception StuckTerm ;;
exception NonBaseTypeResult;;

open Printf;;

(*Types of machineLang *)
type machType = machInt | machWord | machFunc of machType * machType

(* Grammar of machineLang *)
type machTerm =
	MtWord of string
	| MtNum of int
	| MtPrefix of machTerm * machTerm
	| MtInsec of machTerm * machTerm
	| MtUnion of machTerm * machTerm

let rec isValue e = match e with
	| MtNum n -> true
	| MtWord w -> true
	| _ -> false
;;

(* Type of Environments *)
type 'a context = Env of (string * 'a) list
type typeContext = machType context
type termContext = machTerm context

let rec lookup env str = match env with
	Env [] -> raise LookupError
	| Env ((name, item) :: gs) ->
		(match (name = str) with
			true -> item
			| false -> lookup (Env (gs)) str
		)
;;

(* Function to add entry to enviroment *)
let rec typeOf env e = match e with
	MtNum n -> MachInt
	| 