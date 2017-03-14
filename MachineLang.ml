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
	| MtLang of string list
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
	| MtOpp(e1,e2,x) -> let v1 = bigEval e1 and v2 = bigEval e2 in
		( match x with
			| MachPrefix -> MtLang(comp_prefix v1 v2)
			| MachUnion -> MtLang(comp_union v1 v2)
			| MachInsec -> MtLang(comp_insec v1 v2)
			| MachConcat -> MtLang(comp_concat v1 v2)
		)
	| _ -> raise StuckTerm
;;

let rec print_list l = match l with 
	| [] -> print_string "[]"
	| h::[] -> print_string h
	| h::t -> print_string h ; print_string ";" ; print_list t
;;

let print_res res = match res with
    | (MtNum n) -> print_int n ; print_string " : Int" 
	| (MtWord w) -> print_string w; print_string " : Word"
    | (MtLang l) -> print_list l ; print_string " : List"
    | _ -> raise NonBaseTypeResult