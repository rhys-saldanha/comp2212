exception LookupError ;;
exception TypeError ;;
exception UnboundVariableError;;
exception Terminated ;;
exception StuckTerm ;;
exception NonBaseTypeResult;;

open Printf;;

(* Types for opperations *)
type machOpp = MachUnion | MachPrefix | MachInsec | MachConcat

(*Types of machineLang *)
type machType = MachInt | MachWord | MachLang | MachFunc of machType * machType

(* Grammar of machineLang *)
type machTerm = MtNum of int
	| MtWord of string
	| MtLang of string
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
	| Env [] -> raise LookupError
	| Env ((name, item) :: gs) ->
		( match (name = str) with
			| true -> item
			| false -> lookup (Env (gs)) str
		)
;;

(* Function to add entry to enviroment *)
let rec typeOf env e = match e with
	| MtNum n -> MachInt
	| MtWord w -> MachWord
	| MtLang l -> MachLang
	| MtOpp (a, b, x) ->
		( match (typeOf env a),(typeOf env b),x with
			| MachWord, MachLang, MachPrefix -> MachLang
			| MachLang, MachLang, MachUnion -> MachLang
			| MachLang, MachLang, MachInsec -> MachLang
			| MachLang, MachLang, MachConcat -> MachLang
			| _ -> raise TypeError
		)
;;

let typeProg e = typeOf (Env []) e ;;

let comp_prefix w l = l;;

let comp_union l1 l2 = l2;;

let comp_insec l1 l2 = l2;;

let comp_concat l1 l2 = l2;;

let rec bigEval e = match e with
	| e when (isValue e) -> e
	| MtOpp (a, b, x) ->	let aEval = bigEval a in
							let bEval = bigEval b in
								( match aEval,bEval,x with
									| MtWord(w),MtLang(l),MachPrefix -> MtLang(comp_prefix w l)
									| MtLang(l1),MtLang(l2),MachUnion -> MtLang(comp_union l1 l2)
									| MtLang(l1),MtLang(l2),MachInsec -> MtLang(comp_insec l1 l2)
									| MtLang(l1),MtLang(l2),MachConcat -> MtLang(comp_concat l1 l2)
									| _ -> raise StuckTerm
								)
	| _ -> raise StuckTerm
;;

let print_res res = match res with
    | (MtNum n) -> print_int n ; print_string " : Int" 
    | (MtWord w) -> print_string w ; print_string " : Word"
    | (MtLang l) -> print_string l ; print_string " : Language"
    | _ -> raise NonBaseTypeResult
