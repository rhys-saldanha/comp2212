(* Types for operations *)
type machOpp = MachUnion | MachPrefix | MachInsec | MachConcat | MachStar | MachGen

(*Types of machineLang *)
type machType = MachInt | MachWord | MachLang | MachNull

(* Grammar of machineLang *)
type machTerm = MtInt of int
	| MtWord of string
	| MtLang of string list
	| MtVar of string
	| MtOpp of machTerm * machTerm * machTerm * machOpp
	| MtAsn of machTerm * machTerm
	| MtPrint of machTerm
	| MtOpen
	| MtRead of machType * machTerm
	| MtNull of string

	
val typeProg : machTerm -> machType
val bigEval : machTerm -> machTerm
val print_res : machTerm -> unit
