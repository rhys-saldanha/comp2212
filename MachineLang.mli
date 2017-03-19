(* Types for opperations *)
type machOpp = MachUnion | MachPrefix | MachInsec | MachConcat | MachStar | MachGen

(*Types of machineLang *)
type machType = MachInt | MachWord | MachLang | MachPrint | MachOpen

(* Grammar of machineLang *)
type machTerm = MtNum of int
	| MtWord of string
	| MtLang of string list
	| MtVar of string
	| MtFile of string
	| MtOpp of machTerm * machTerm * machTerm * machOpp
	| MtAsn of machTerm * machTerm
	| MtPrint of machTerm
	| MtOpen of machTerm
	
val typeProg : machTerm -> machType
val bigEval : machTerm -> machTerm
val print_res : machTerm -> unit
