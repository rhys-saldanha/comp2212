exception LookupError ;;
exception TypeError ;;
exception UnboundVariableError;;
exception Terminated ;;
exception StuckTerm ;;
exception NonBaseTypeResult;;

open Printf;;

(* Types for opperations *)
type machOpp = MachUnion | MachPrefix | MachInsec

(*Types of machineLang *)
type machType = MachInt | MachWord | MachLang | MachFunc of machType * machType

(* Grammar of machineLang *)
type machTerm = MtWord of string
	| MtLang of string
	| MtNum of int
	| MtOpp of machTerm * machTerm * machOpp

let rec isValue e = match e with
	| MtNum n -> true
	| MtWord w -> true
	| MtLang l -> true
	| _ -> false
;;

(* Type of Environments *)
type 'a context = Env of (string * 'a) list
type typeContext = machType context
type termContext = machTerm context

let rec lookup env str = match env with
	Env [] -> raise LookupError
	| Env ((name, item) :: gs) ->
		( match (name = str) with
			true -> item
			| false -> lookup (Env (gs)) str
		)
;;

(* Function to add entry to enviroment *)
let rec typeOf env e = match e with
	MtNum n -> MachInt
	| MtWord w -> MachWord
	| MtLang l -> MachLang
	| MtOpp (a, b, x) ->
		( match (typeOf env a) , (typeOf env b) , x with
			MachWord, MachLang, MachPrefix -> MachLang
			|	MachLang, MachLang, MachUnion -> MachLang
			|	MachLang, MachLang, MachInsec -> MachLang
			| _ -> raise TypeError
		)
;;

let typeProg e = typeOf (Env []) e ;;
