(* Types for opperations *)
type machOpp = MachUnion | MachPrefix | MachInsec | MachConcat | MachStar | MachGen

(*Types of machineLang *)
type machType = MachInt | MachWord | MachLang | MachFunc of machType * machType

(* Grammar of machineLang *)
type machTerm = MtNum of int
	| MtWord of string
	| MtLang of string list
	| MtVar of string
	| MtAsn of machTerm * machTerm
	| MtOpp of machTerm * machTerm * machTerm * machOpp
	
val typeProg : machTerm -> machType
val bigEval : machTerm -> machTerm
val print_res : machTerm -> unit
