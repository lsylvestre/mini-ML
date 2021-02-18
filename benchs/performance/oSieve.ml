(* $Id: oSieve.ml,v 1.2 2006/07/16 13:39:32 emmanuel Exp $ *)


let succ x = x + 1;;
(* Eratosthene's sieve *)

(* interval min max = [min; min+1; ...; max-1; max] *)

let rec interval min max =
  if min > max then [] else min :: interval (succ min) max
;;

(* filter p L returns the list of the elements in list L
   that satisfy predicate p *)

let rec filter p l = match l with
    []   -> []
  | (a::r) -> if p a then a :: filter p r else filter p r
;;

(* Application: removing all numbers multiple of n from a list of integers *)

let remove_multiples_of n =
  filter (fun m -> if (m mod n) = 0 then false else true)
;;

(* The sieve itself *)

let sieve max =
  let rec filter_again = function
     [] -> []
  | n::r as l ->
      if n*n > max then l else n :: filter_again (remove_multiples_of n r)
  in
    filter_again (interval 2 max)
;;

let rec do_list f l = match l with 
       []   -> ()
  |  (a::q) -> f a; do_list f q
;;
 for i = 1 to 10 do 
do_list (fun n -> print_int n; print_string " ") (sieve 250000);
print_string "\n" done;;
