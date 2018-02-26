(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)

fun only_capitals strlist =
    List.filter (fn x => Char.isUpper(String.sub(x, 0))) strlist

fun longest_string1 strlist =
    List.foldl (fn (x,y) => if String.size(x) > String.size(y) then x else y) "" strlist

fun longest_string2 strlist =
    List.foldl (fn (x,y) => if String.size(x) >= String.size(y) then x else y) "" strlist

fun longest_string_helper f strlist =
    List.foldl (fn(x,y) => if f(String.size(x),String.size(y)) then x else y) "" strlist

val longest_string3 = longest_string_helper (fn (x,y) => x > y)
					    
val longest_string4 = longest_string_helper (fn (x,y) => x >= y)

val longest_capitalized = longest_string1 o only_capitals

val rev_string = String.implode o List.rev o String.explode

fun first_answer f alist =
    case alist of
	[] => raise NoAnswer
      | a::alist' => case f(a) of
			 SOME v => v
		       | NONE => first_answer f alist'

fun all_answers f alist =
    let fun helper_fun (alist, acc) =
	case alist of
	    [] => SOME acc
	  | a::alist' => case f(a) of
			     SOME x => helper_fun (alist', x @ acc)
			   | NONE => NONE				 
    in
	helper_fun(alist, [])
    end

fun count_wildcards pattern =
    let fun f1 () = 1
	fun f2 x = 0
    in
	g f1 f2 pattern
    end

fun count_wild_and_variable_lengths pattern =
    let fun f1 () = 1
	fun f2 x = String.size(x)
    in
	g f1 f2 pattern
    end
	
fun count_some_var (str, pattern) =
    let fun f1 () = 0
	fun f2 x = if x = str then 1 else 0
    in
	g f1 f2 pattern
    end

fun check_pat pattern =
    let fun all_strings pattern =
	    case pattern of
		Variable x => [x]
	      | TupleP ps => List.foldl (fn (p,i) => all_strings(p) @ i) [] ps
	      | _ => []
	fun isRepeated strList =
	    case strList of
		[] => true
	      | x::xs' => if List.exists (fn str => str = x) xs' then false else isRepeated xs'
    in
	isRepeated(all_strings pattern)
    end

fun match (valu, pattern) =
    case (valu, pattern) of
	(_, Wildcard) => SOME []
      | (_, Variable s) => SOME [(s, valu)]
      | (Unit, UnitP) => SOME []
      | (Const a, ConstP b) => if a = b then SOME [] else NONE
      | (Tuple vs, TupleP ps) => if (List.length vs = List.length ps)
				 then all_answers match (ListPair.zip (vs, ps))
				 else NONE
      | (Constructor (s1,v), ConstructorP (s2,p)) => if s1 = s2 then match(v,p) else NONE
      | _ => NONE 

fun first_match valu pat_list =
    SOME (first_answer (fn p => match(valu, p)) pat_list)
    handle NoAnswer => NONE
