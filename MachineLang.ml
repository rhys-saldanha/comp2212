exception LookupError ;;
exception TypeError of string;;
exception Terminated ;;
exception StuckTerm ;;
exception NonBaseTypeResult;;

open Printf;;
open List;;
open String;;

(* Types for opperations *)
type machOpp = MachUnion | MachPrefix | MachInsec | MachConcat | MachStar

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
let typeToString x = match x with 
	| MachInt -> "Int"
	| MachWord -> "Word"
	| MachLang -> "Language"
	| _ -> "Unknown type"
;;

let makeTypeError x y z = "(" ^ (typeToString x) ^ ", " ^ (typeToString y) ^ ", " ^ (typeToString z) ^ ")";;

(* Function to add entry to enviroment *)
let rec typeOf e = match e with
	| MtNum n -> MachInt
	| MtWord w -> MachWord
	| MtLang l -> MachLang
	| MtOpp (a, b, n, x) ->
		( match x with
			| MachPrefix ->
				( match (typeOf a),(typeOf b),(typeOf n) with
					| MachWord, MachLang, MachInt -> MachLang
					| x,y,z -> raise (TypeError ("Prefix was expecting (Word, Language, Int), received \n" ^ makeTypeError x y z))
				)
			| MachUnion ->
				( match (typeOf a),(typeOf b),(typeOf n) with
					| MachLang, MachLang, MachInt -> MachLang
					| x,y,z -> raise (TypeError ("Union was expecting (Language, Language, Int), received \n" ^ makeTypeError x y z))
				)
			| MachInsec ->
				( match (typeOf a),(typeOf b),(typeOf n) with
					| MachLang, MachLang, MachInt -> MachLang
					| x,y,z -> raise (TypeError ("Insec was expecting (Language, Language, Int), received \n" ^ makeTypeError x y z))
				)
			| MachConcat ->
				( match (typeOf a),(typeOf b),(typeOf n) with
					| MachLang, MachLang, MachInt -> MachLang
					| x,y,z -> raise (TypeError ("Concat was expecting (Language, Language, Int), received \n" ^ makeTypeError x y z))
				)
		)
;;

let typeProg e = typeOf e ;;

let rec sublist l n = match l,n with
	| _, 0 -> []
	| h::t, _ -> h::sublist t (n-1)
	| [],_ -> []
;;

let rec uniq l = match l with
	| h::t -> h::uniq(filter (fun x -> x<>h) t)
	| [] -> []
;;

let sort_uniq c l = sort c (uniq l);;

let tidy_lang l n = 
	sublist (sort_uniq compare l) n
;;

let comp_prefix w l n =
	tidy_lang (List.map (fun x -> w^x) l) n
;;

let comp_union l1 l2 n =
	tidy_lang (l1 @ l2) n
;;

let rec insec l1 l2cl = match l1 with
	| h::t -> (filter (fun x -> x=h) l2) @ insec t l2
	| [] -> []
;;

let comp_insec l1 l2 n = 
	tidy_lang (insec l1 l2) n
;;

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
	| [] -> print_string ""
	| h::[] -> print_string h
	| h::t -> print_string h ; print_string ", " ; print_list t
;;

let print_res res = match res with
    | (MtNum n) -> print_int n ; print_string " : Int" 
	| (MtWord w) -> print_string w; print_string " : Word"
    | (MtLang l) -> print_string "{" ; print_list l ; print_string "} : List"
    | _ -> raise NonBaseTypeResult