exception LookupError ;;
exception TypeError ;;
exception UnboundVariableError;;
exception Terminated ;;
exception StuckTerm ;;
exception NonBaseTypeResult;;

open Printf;;
open List;;
open String;;

(* Types for opperations *)
type machOpp = MachUnion | MachPrefix | MachInsec | MachConcat

(*Types of machineLang *)
type machType = MachInt | MachWord | MachLang | MachFunc of machType * machType

(* Grammar of machineLang *)
type machTerm = MtNum of int
	| MtWord of string
	| MtLang of string list
	| MtOpp of machTerm * machTerm * machTerm * machOpp

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
	| MtOpp (a, b, n, x) ->
		( match (typeOf env a),(typeOf env b),(typeOf env n),x with
			| MachWord, MachLang, MachInt, MachPrefix -> MachLang
			| MachLang, MachLang, MachInt, MachUnion -> MachLang
			| MachLang, MachLang, MachInt, MachInsec -> MachLang
			| MachLang, MachLang, MachInt, MachConcat -> MachLang
			| _ -> raise TypeError
		)
;;

let typeProg e = typeOf (Env []) e ;;

let rec sublist l n = match l,n with
	| _, 0 -> []
	| h::t, _ -> h::sublist t (n-1)
	| [],_ -> []
;;

let tidy_lang l n = 
	let r = ref [] in
	r := sort_uniq compare l;
	r := sublist !r n;
	!r
;;

let comp_prefix w l n =
	let r = ref [] in 
	r := tidy_lang l n;
	r := List.map (fun x -> w^x) !r;
	tidy_lang !r n
;;

let comp_union l1 l2 n = 
	let r1 = ref [] and r2 = ref [] in
	r1 := tidy_lang l1 n;
	r2 := tidy_lang l2 n;
	r1 := !r1 @ !r2;
	tidy_lang !r1 n
;;

let comp_insec l1 l2 n = l2;;

let comp_concat l1 l2 n = l2;;

let rec bigEval e = match e with
	| e when (isValue e) -> e
	| MtOpp(e1,e2,i,x) -> let v1 = bigEval e1 and v2 = bigEval e2 and v3 = bigEval i in
		( match v1,v2,v3,x with
			| MtWord(w),MtLang(l),MtNum(n),MachPrefix -> MtLang(comp_prefix w l n)
			| MtLang(l1),MtLang(l2),MtNum(n),MachUnion -> MtLang(comp_union l1 l2 n)
			| MtLang(l1),MtLang(l2),MtNum(n),MachInsec -> MtLang(comp_insec l1 l2 n)
			| MtLang(l1),MtLang(l2),MtNum(n),MachConcat -> MtLang(comp_concat l1 l2 n)
			| _ -> raise StuckTerm
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