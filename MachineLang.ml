exception LookupError ;;
exception TypeError of string;;
exception InputError of string;;
exception Terminated ;;
exception StuckTerm ;;
exception NonBaseTypeResult;;

open Printf;;
open List;;
open String;;

(* Types for opperations *)
type machOpp = MachUnion | MachPrefix | MachInsec | MachConcat | MachStar | MachGen

(*Types of machineLang *)
type machType = MachInt | MachWord | MachLang | MachPrint

(* Grammar of machineLang *)
type machTerm = MtNum of int
	| MtWord of string
	| MtLang of string list
	| MtVar of string
	| MtAsn of machTerm * machTerm
	| MtPrint of machTerm
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

let envType = ref (Env []);;
let envVal = ref (Env []);;

let rec lookup env str = match env with
	| Env [] -> raise LookupError
	| Env ((x, e) :: gs) ->
		( match (x = str) with
			| true -> e
			| false -> lookup (Env (gs)) str
		)
;;
(* Function to add an extra type entry in to an environment *)
let addBindingType x t  = match !envType with 
      Env(gs) -> envType := Env ((x, t) :: gs); t
	  
;;(* Function to add an extra value entry in to an environment *)
let addBindingVal x v  = match !envVal with 
      Env(gs) -> envVal := Env ((x, v) :: gs); v
;;

let typeToString x = match x with 
	| MachInt -> "Int"
	| MachWord -> "Word"
	| MachLang -> "Language"
	| _ -> "Unknown type"
;;
let makeTypeError3 x y z = "(" ^ (typeToString x) ^ ", " ^ (typeToString y) ^ ", " ^ (typeToString z) ^ ")";;
let makeTypeError2 x y = "(" ^ (typeToString x) ^ ", " ^ (typeToString y) ^ ")";;

(* Function to add entry to enviroment *)
let rec typeOf env e = match e with
	| MtNum n -> MachInt
	| MtWord w -> MachWord
	| MtLang l -> MachLang
	| MtOpp (a, b, n, x) ->
		( match x with
			| MachPrefix ->
				( match (typeOf env a),(typeOf env b),(typeOf env n) with
					| MachWord, MachLang, MachInt -> MachLang
					| x,y,z -> raise (TypeError ("prefix was expecting (Word, Language, Int), received \n\t" ^ makeTypeError3 x y z))
				)
			| MachUnion ->
				( match (typeOf env a),(typeOf env b),(typeOf env n) with
					| MachLang, MachLang, MachInt -> MachLang
					| x,y,z -> raise (TypeError ("union was expecting (Language, Language, Int), received \n\t" ^ makeTypeError3 x y z))
				)
			| MachInsec ->
				( match (typeOf env a),(typeOf env b),(typeOf env n) with
					| MachLang, MachLang, MachInt -> MachLang
					| x,y,z -> raise (TypeError ("insec was expecting (Language, Language, Int), received \n\t" ^ makeTypeError3 x y z))
				)
			| MachConcat ->
				( match (typeOf env a),(typeOf env b),(typeOf env n) with
					| MachLang, MachLang, MachInt -> MachLang
					| x,y,z -> raise (TypeError ("concat was expecting (Language, Language, Int), received \n\t" ^ makeTypeError3 x y z))
				)
			| MachStar ->
				( match (typeOf env a), (typeOf env n) with
					| MachLang, MachInt -> MachLang
					| x,y -> raise (TypeError ("star was expecting (Language, Int), received \n\t" ^ makeTypeError2 x y))
				)
			| MachGen ->
				( match (typeOf env a), (typeOf env n) with
					| MachLang, MachInt -> MachLang
					| x,y -> raise (TypeError ("gen was expecting (Language, Int), received \n\t" ^ makeTypeError2 x y))
				)
		)
	| MtVar (x) -> (try lookup !envType x with LookupError -> raise (TypeError (x ^ " is unbound")))
	| MtAsn (e1, e2) -> 
		( match e1 with
			| MtVar x -> addBindingType x (typeOf env e2)
			| _ -> raise (InputError "Variable name not allowed")
		)
	| MtPrint _ -> MachPrint
;;

let typeProg e = typeOf (Env []) e ;;

let rec sublist l n = match l,n with
	| _, 0 -> []
	| h::t, _ -> h::sublist t (n-1)
	| [],_ -> []
;;

let rec uniq l = match l with
	| h::t -> h::uniq(List.filter (fun x -> x<>h) t)
	| [] -> []
;;

let sort_uniq c l = sort c (uniq l);;

let tidy_lang l n = 
	sublist (sort_uniq String.compare l) n
;;

(* PREFIX *)
let comp_prefix w l n =
	tidy_lang (List.map (fun x -> w^x) l) n
;;
(* ---------- *)

(* UNION *)
let comp_union l1 l2 n =
	tidy_lang (l1 @ l2) n
;;
(* ---------- *)

(* INSEC *)
let rec insec l1 l2 = match l1 with
	| h::t -> (List.filter (fun x -> x=h) l2) @ insec t l2
	| [] -> []
;;
let comp_insec l1 l2 n = 
	tidy_lang (insec l1 l2) n
;;
(* ---------- *)

(* CONCAT *)
let rec concat l1 l2 n = match l1 with
	| h::[] -> (comp_prefix h l2 n)
	| h::t -> (comp_prefix h l2 n) @ (concat t l2 n)
	| [] -> l2
;;
let comp_concat l1 l2 n = tidy_lang (concat l1 l2 n) n;;
(* ---------- *)

(* STAR *)
let rec repeat w n = match n with
	| 0 -> ""
	| _ -> w ^ (repeat w (n-1))
;;
let rec star w n = match n with
	| 0 -> [""]
	| _ -> (repeat w n) :: (star w (n-1))
;;
let rec starAll l n = match l with
	| h::t -> (star h n) @ (starAll t n)
	| [] -> []
;;
let comp_star l n = match l with
	| _::[] -> tidy_lang (starAll l n) n
	| _ -> raise (InputError "Kleene-star operation requires non-empty language")
;;
(* ---------- *)

(* GEN *)
let checkChar l1 = 
	let l2 = List.filter (fun x -> (String.length x) = 1) l1 in
		let r = ((List.length l1) - (List.length l2) = 0) in
			( match r with
				| true -> l1
				| false -> raise (InputError "Language gen operation requires a language of characters")
			)
;;

let rec gen l n = match n with 
	| 0 -> [""]
	| _ -> 
		let g = (gen l (n-1)) and r = ref [] in
			List.iter (fun c -> r := !r @ (List.map (fun w -> c^w) g)) l;
			!r
;;
let comp_gen l1 n =
	let l = checkChar l1 in
		gen (sort_uniq String.compare l) n
;;
(* ---------- *)

let rec bigEval e = match e with
	| MtVar (x) -> lookup !envVal x
	| e when (isValue e) -> e
	| MtOpp(e1,e2,i,x) -> let v1 = bigEval e1 and v2 = bigEval e2 and v3 = bigEval i in
		( match v1,v2,v3,x with
			| MtWord(w),MtLang(l),MtNum(n),MachPrefix -> MtLang(comp_prefix w l n)
			| MtLang(l1),MtLang(l2),MtNum(n),MachUnion -> MtLang(comp_union l1 l2 n)
			| MtLang(l1),MtLang(l2),MtNum(n),MachInsec -> MtLang(comp_insec l1 l2 n)
			| MtLang(l1),MtLang(l2),MtNum(n),MachConcat -> MtLang(comp_concat l1 l2 n)
			| MtLang(l1),MtLang(l2),MtNum(n),MachStar -> MtLang(comp_star l1 n)
			| MtLang(l1),MtLang(l2),MtNum(n),MachGen -> MtLang(comp_gen l1 n)
			| _ -> raise StuckTerm
		)
	| MtAsn(MtVar(x), v) -> addBindingVal x (bigEval v)
	| MtPrint x -> MtPrint (bigEval x)
	| _ -> raise StuckTerm
;;

let rec print_list l = match l with 
	| [] -> print_string ""
	| ""::t -> print_list (":"::t)
	| h::[] -> print_string h
	| h::t -> print_string h ; print_string ", " ; print_list t
;;
let print_mt res = match res with
    | (MtNum n) -> print_int n ; print_string " : Int\n" 
	| (MtWord w) -> print_string w; print_string " : Word\n"
    | (MtLang l) -> print_string "{" ; print_list l ; print_string "} : List\n"
    | _ -> raise NonBaseTypeResult
;;
let print_res res = match res with
	| MtPrint x -> print_mt x
	| _ -> print_string ""