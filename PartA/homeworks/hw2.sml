(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

fun all_except_option (str, str_lst) =
    case str_lst of
	[] => NONE
      | hd::tl => if same_string (str, hd) then SOME tl
		  else let val ans = all_except_option(str, tl)
		       in case ans of
			      SOME a => SOME (hd :: a)
			    | NONE => NONE
		       end

fun get_substitutions1 (substitutions, name) =
    case substitutions of
	[] => []
      | subList::tl => case all_except_option(name, subList) of
			   SOME lst => lst @ get_substitutions1(tl, name)
			 | NONE => get_substitutions1(tl, name)

fun get_substitutions2 (substitutions, name) =
    let fun f (substitutions, acc) =
	    case substitutions of
		[] => acc
	      | subList::tl => case all_except_option(name, subList) of
				   SOME lst => f(tl, acc @ lst)
				 | NONE => f(tl, acc)					   
    in f (substitutions, [])
    end

fun similar_names (substitutions, { first=x, middle=y, last=z }) =
    let val all_names = x :: get_substitutions2(substitutions, x)
	fun f (rem_substitutes, acc) =
	    case rem_substitutes of
		[] => acc
	       | name::rem_names => f (rem_names, acc @ [{first=name, middle=y, last=z}] )
    in f (all_names, [])
    end

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw

exception IllegalMove

(* put your solutions for problem 2 here *)

fun card_color card =
    case card of
	(Clubs, _) => Black
      | (Spades, _) => Black
      | _ => Red
		 
fun card_value card =
    case card of
	(_, Num x) => x
      | (_, Ace) => 11
      | _ => 10

fun remove_card (cs, c, e) =
    case cs of
	[] => raise e
      | x::xs' => if x = c then xs' else x :: remove_card(xs', c, e) 

fun all_same_color cs =
    case cs of
	[] => true
      | _::[] => true		     
      | head::(neck::rest) => (card_color head = card_color neck andalso all_same_color (neck::rest))
				  
fun sum_cards cs =
    let fun f (cs, acc) =
	    case cs of
		[] => acc
	      | hd::tl => f (tl, acc + card_value hd)
    in
	f (cs, 0)
    end

fun score (cs, goal) =
    let val sum = sum_cards cs
	val prelim_score = if sum > goal then ( 3 * (sum - goal)) else (goal - sum)
    in
	if all_same_color cs then (prelim_score div 2) else prelim_score
    end

fun officiate (cs, mv_list, goal) =
    let fun f (cs, mv_list, held_cs) =
	    if sum_cards held_cs > goal
	    then score(held_cs, goal)
	    else				 
		case mv_list of
		    [] => score(held_cs, goal)
		  | mv::mv_list' => case mv of
					Discard cd => f (cs, mv_list', remove_card(held_cs, cd, IllegalMove))
				      | Draw => case cs of
						    [] => score(held_cs, goal)
						  | cd::cs' => f (cs', mv_list', cd::held_cs)					
    in
	f (cs, mv_list, [])
    end
