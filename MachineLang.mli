exception TypeError of string;;
exception InputError of string;;
exception EvalError of string;;
exception StuckTerm ;;
exception PrintError of string;;

(* Types for operations *)
type machOpp = MachUnion | MachPrefix | MachInsec | MachConcat | MachStar | MachGen | MachReduc

(*Types of machineLang *)
type machType = MachInt | MachWord | MachLang | MachNull

(* Grammar of machineLang *)
type machTerm = MtInt of int
	| MtWord of string
	| MtLang of string list
	| MtVar of string
	| MtOpp of machTerm * machTerm * machOpp
	| MtAsn of machTerm * machTerm
	| MtPrint of machTerm
	| MtOpen
	| MtRead of machType * machTerm
	| MtNull of string

	
val typeProg : machTerm -> machType
val bigEval : machTerm -> machTerm
val print_res : machTerm -> unit
