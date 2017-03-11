type machOpp = MachUnion | MachPrefix | MachInsec

type machType = MachInt | MachWord | MachLang | MachFunc of machType * machType

type machTerm = MtWord of string
	| MtLang of string
	| MtNum of int
	| MtOpp of machTerm * machTerm * machOpp
	
val typeProg : machTerm -> machType
