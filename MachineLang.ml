exception LookupError ;;
exception TypeError of string;;
exception InputError of string;;
exception EvalError of string;;
exception Terminated ;;
exception StuckTerm ;;
exception NonBaseTypeResult;;

open Printf;;
open List;;
open String;;

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

let rec isValue e = match e with
	| MtInt n -> true
	| MtWord w -> true
	| MtLang l -> true
	| _ -> false
;;

(* Type of Environments *)
type 'a context = Env of (string * 'a) list

let envType = ref (Env []);;
let envVal = ref (Env []);;
let lines = ref [];;

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
let makeTypeError1 x = "(" ^ (typeToString x) ^ ")";;

(* Function to add entry to enviroment *)
let rec typeOf env e = match e with
	| MtInt n -> MachInt
	| MtWord w -> MachWord
	| MtLang l -> MachLang
	| MtOpp (a, b, x) ->
		( match x with
			| MachPrefix ->
				( match (typeOf env a),(typeOf env b) with
					| MachWord, MachLang -> MachLang
					| x,y -> raise (TypeError ("\nprefix was expecting \n\t(Word, Language)\nreceived \n\t" ^ makeTypeError2 x y))
				)
			| MachUnion ->
				( match (typeOf env a),(typeOf env b) with
					| MachLang, MachLang -> MachLang
					| x,y -> raise (TypeError ("union was expecting \n\t(Language, Language)\nreceived \n\t" ^ makeTypeError2 x y))
				)
			| MachInsec ->
				( match (typeOf env a),(typeOf env b) with
					| MachLang, MachLang -> MachLang
					| x,y -> raise (TypeError ("insec was expecting \n\t(Language, Language)\nreceived \n\t" ^ makeTypeError2 x y))
				)
			| MachConcat ->
				( match (typeOf env a),(typeOf env b) with
					| MachLang, MachLang -> MachLang
					| x,y -> raise (TypeError ("concat was expecting \n\t(Language, Language)\nreceived \n\t" ^ makeTypeError2 x y))
				)
			| MachStar ->
				( match (typeOf env a), (typeOf env b) with
					| MachLang, MachInt -> MachLang
					| x,y -> raise (TypeError ("star was expecting \n\t Language, Int)\nreceived \n\t" ^ makeTypeError2 x y))
				)
			| MachGen ->
				( match (typeOf env a), (typeOf env b) with
					| MachLang, MachInt -> MachLang
					| x,y -> raise (TypeError ("gen was expecting \n\t(Language, Int)\nreceived \n\t" ^ makeTypeError2 x y))
				)
			| MachReduc ->
				( match (typeOf env a), (typeOf env b) with
					| MachLang, MachInt -> MachLang
					| x,y -> raise (TypeError ("reduce was expecting \n\t(Language, Int)\nreceived \n\t" ^ makeTypeError2 x y))
				)
		)
	| MtVar (x) -> (try lookup !envType x with LookupError -> raise (TypeError ("Variable " ^ x ^ " is unbound")))
	| MtAsn (e1, e2) -> 
		( match e1 with
			| MtVar x -> addBindingType x (typeOf env e2)
			| _ -> raise (InputError "Variable name not allowed (are you assigning a value to another value?)")
		)
	| MtPrint (x) -> ignore (typeOf env x); MachNull
	| MtOpen -> MachNull
	| MtRead (t,n) -> t
	| MtNull _ -> MachNull
;;

let typeProg e = typeOf (Env []) e ;;

(* REDUCE *)
let rec sublist l n = match l,n with
	| _, 0 -> []
	| h::t, _ -> h::sublist t (n-1)
	| [],_ -> []
;;

let rec uniq l = match l with
	| h::t -> h::uniq(List.filter (fun x -> x<>h) t)
	| [] -> []
;;

let sort_uniq l = sort String.compare (uniq l);;

let comp_reduc l n = match n < 1 with
	| true -> sort_uniq l
	| false -> sublist (sort_uniq l) n
;;
(* ---------- *)

(* PREFIX *)
let comp_prefix w l =
	sort_uniq (List.map (fun x -> w^x) l)
;;
(* ---------- *)

(* UNION *)
let comp_union l1 l2 =
	sort_uniq (l1 @ l2)
;;
(* ---------- *)

(* INSEC *)
let rec insec l1 l2 = match l1 with
	| h::t -> (List.filter (fun x -> x=h) l2) @ insec t l2
	| [] -> []
;;
let comp_insec l1 l2 = 
	sort_uniq (insec l1 l2)
;;
(* ---------- *)

(* CONCAT *)
let rec concat l1 l2 = match l1 with
	| h::[] -> (comp_prefix h l2)
	| h::t -> (comp_prefix h l2) @ (concat t l2)
	| [] -> l2
;;
let comp_concat l1 l2 = sort_uniq (concat l1 l2);;
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
	| _::[] -> comp_reduc (starAll l n) n
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
		gen (sort_uniq l) n
;;
(* ---------- *)

(* OPEN *)
let comp_open chan =
	try
		while true; do
			lines := input_line chan :: !lines
		done; ""
	with End_of_file ->
		close_in chan;
		lines := List.rev !lines;
		""
;;
(* ---------- *)

(* READ *)
let f r x c =
	( match x = c with
		| true -> (r := "" :: !r)
		| false ->
			( match !r with
				| [] -> (r := (""^(String.make 1 x)) :: [])
				| _ -> (r := ((List.hd !r)^(String.make 1 x)) :: (List.tl !r))
			)
	)
;;
let split c s = 
	let res = ref [] in
		(String.iter (fun x -> f res x c) s);
		!res;
;;
let read_lang n =	split ',' (Str.global_replace (Str.regexp "[' ' '\t' '{' '}' ':']") "" (List.nth !lines n))
;;

let comp_read_int n = try
	int_of_string (List.nth !lines n) 
	with Failure "int_of_string" -> raise (InputError "readline could not parse int from file")
		| Failure "nth" -> raise (InputError ("line number for readline INT " ^ (string_of_int n) ^ " not found"))
		| _ -> raise (EvalError ("could not evaluate readline INT " ^ (string_of_int n)))
;;

let comp_read_word n = try
	List.nth !lines n
	with Failure "nth" -> raise (InputError ("line number for readline WORD " ^ (string_of_int n) ^ " not found"))
		| _ -> raise (EvalError ("could not evaluate readline WORD " ^ (string_of_int n)))
;;

let comp_read_lang n = try
	( match String.contains (List.nth !lines n) ':' with
		| false -> read_lang n
		| true -> read_lang n @ [""]
	)
	with Failure "nth" -> raise (InputError ("line number for readline LANG " ^ (string_of_int n) ^ " not found"))
		| _ -> raise (EvalError ("could not evaluate readline LANG " ^ (string_of_int n)))
;;
(* ---------- *)

let rec bigEval e = match e with
	| MtVar (x) -> lookup !envVal x
	| e when (isValue e) -> e
	| MtOpp(e1,e2,x) -> let v1 = bigEval e1 and v2 = bigEval e2 in
		( match v1,v2,x with
			| MtWord(w),MtLang(l),MachPrefix -> MtLang(comp_prefix w l)
			| MtLang(l1),MtLang(l2),MachUnion -> MtLang(comp_union l1 l2)
			| MtLang(l1),MtLang(l2),MachInsec -> MtLang(comp_insec l1 l2)
			| MtLang(l1),MtLang(l2),MachConcat -> MtLang(comp_concat l1 l2)
			| MtLang(l1),MtInt(n),MachStar -> MtLang(comp_star l1 n)
			| MtLang(l1),MtInt(n),MachGen -> MtLang(comp_gen l1 n)
			| MtLang(l1),MtInt(n),MachReduc -> MtLang(comp_reduc l1 n)
			| _ -> raise StuckTerm
		)
	| MtAsn(MtVar(x), v) -> addBindingVal x (bigEval v)
	| MtPrint x -> MtPrint (bigEval x)
	| MtOpen -> MtNull(comp_open stdin)
	| MtRead(t,MtInt(n)) ->
		( match t with
			| MachInt -> MtInt(comp_read_int n)
			| MachWord -> MtWord(comp_read_word n)
			| MachLang -> MtLang(comp_read_lang n)
			| _ -> raise StuckTerm
		)
	| _ -> raise StuckTerm
;;

let rec print_list l = match l with 
	| [] -> print_string ""
	| ""::t -> print_list (":"::t)
	| h::[] -> print_string h
	| h::t -> print_string h ; print_string ", " ; print_list t
;;
let print_mt res = match res with
    | (MtInt n) -> print_int n;
	| (MtWord w) -> print_string w;
    | (MtLang l) -> print_string "{" ; print_list l ; print_string "}"
    | _ -> raise NonBaseTypeResult
;;
let print_res res = match res with
	| MtPrint x -> print_mt x; print_newline ()
	| _ -> print_string ""
;;	