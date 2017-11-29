(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

fun all_except_option (str_to_match, str_list) =
  (*Keep track of two lists, the strings we've seen that don't match
    and the rest of the rest we are iterating through (str_list).
    If we encounter the same string we combine the old strings with
    the rest of rest of the str_list skipping the curr string, head,
    which is the same string as we passed in. *)
  let fun helper (str_list, acc) = 
        case str_list of
            [] => NONE
         |  s::ss' => if same_string(s, str_to_match)
                      then SOME (acc @ ss')
                      else helper(ss', s::acc)
  in
      helper(str_list, [])
  end

fun get_substitutions1 (sll, s) =
  case sll of 
      [] => []
   |  hd::sll' => case all_except_option(s, hd) of 
                      NONE => get_substitutions1(sll', s)
                   |  SOME i => i @ get_substitutions1(sll',s)
      
fun get_substitutions2 (sll, s) =
  let fun helper (sll, acc) =
        case sll of 
            [] => acc
         |  hd::sll' => case all_except_option(s, hd) of 
                            NONE => helper(sll', acc)
                         |  SOME i => helper(sll', i @ acc)
  in
      helper(sll, [])
  end

fun similar_names (sll, {first=first_name, middle=middle_name, last=last_name}) =
  let val first_names = get_substitutions2(sll,first_name)
  in
      let fun helper (str_list, acc) =
            case str_list of
                [] => acc
              | s::str_list' => helper(str_list', {first=s, middle=middle_name, last=last_name}::acc)
      in
          helper(first_names,[{first=first_name, middle=middle_name, last=last_name}])
      end
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
                        
fun card_color (suit, rank) =
  case suit of
      Diamonds => Red
    | Hearts => Red
    | Spades => Black                   
    | Clubs => Black                   

fun card_value (_, rank) = 
  case rank of
      Num i => i
   |  Ace  => 11
   |  Jack => 10
   |  King => 10
   |  Queen=> 10

fun remove_card (cs, c, e) =
  let fun helper (cs, acc) =
        case cs of 
            [] => raise e
          | card::cs' =>  if card = c 
                          then acc @ cs' (*add rest of card to acc and finish (we skip adding this one)*)
                          else helper(cs', card::acc)
  in
      helper(cs, [])
  end
        
fun all_same_color (cs) =
  let val color = 
          case cs of 
              [] => Black
            | c::cs' => card_color (c) 
  in
      let fun helper (color, cs) =
            case cs of
                [] => true
              | c::cs' => if card_color(c) = color  
                          then helper(color, cs')
                          else false
      in
          helper(color, cs)
      end
  end

fun sum_cards (cs) =
  let fun helper (cs, acc) =
        case cs of
            [] => acc
          | c::cs' => helper(cs',card_value (c) + acc) 
  in
      helper(cs, 0)
  end

fun score (cs, goal) =
  let val initial_sum = sum_cards cs
      val prelim_sum  = if initial_sum > goal
                        then (3 * (initial_sum - goal))
                        else (goal - initial_sum)                       
  in
      if all_same_color(cs)
      then (prelim_sum div 2)
      else prelim_sum
  end

fun officiate (cs, ms, goal) = 
  let fun run (player_cards, moves, deck) = 
        if sum_cards(player_cards) > goal
        then score(player_cards, goal)
        else
            case moves of
                [] => score (player_cards, goal)
             |  m::ms' => case m of
                              Discard curr => run (remove_card(player_cards, curr, IllegalMove), ms', deck)
                            | Draw => case deck of                           
                                          [] => score (player_cards, goal)
                                        | top::deck' => run(top::player_cards, ms', deck') 
  in
      run ([], ms, cs)
  end                                                         
