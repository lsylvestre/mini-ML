(* $Id: aQueens.ml,v 1.2 2006/07/16 13:39:32 emmanuel Exp $ *)

(* Benchmark on list allocation and manipulation

fichier queens.ml Pierre Weis

*)

(*let succ x = x + 1;;
*)

let rec append l1 l2 = match l1 with 
  [] -> l2
| a::q -> a::(append q l2);;

let rec map f l = match l with [] -> [] | h::t -> (f h)::(map f t);;

let rec interval n m =
    if n > m then [] else (n :: interval (succ n) m);;

let rec concmap f = function
    [] -> []
  | x :: l -> append (f x) (concmap f l ) (*f x @ concmap f l*);;

let rec list_length = function
    [] -> 0
  | _::l -> 1 + list_length l;;

let rec safe d x = function
    [] -> true
  | q::l -> (not (x = q)) & ((not (x = q+d)) & ((not (x = q-d)) &
            safe (d+1) x l));;

let ok = function [] -> true | x::l -> safe 1 x l;;

let rec filter p = function
   [] -> []
 | x::l -> if p x then x::filter p l else filter p l;;
let range = interval 1;;

let print_bool b = print_int b;;
let print_bl bl = map print_bool bl;;
let print_bll bll = map (fun bl -> print_bl bl; print_newline()) bll;;
 
let queens n =
 let qs = range n in
 let testcol = function b -> filter ok (map (fun q -> q::b) qs) in
 let rec gen = function
    0 -> [[]]
  | n -> print_string "\n** : ";  print_int n;print_newline();
           let r = concmap testcol (gen (n - 1)) in
           (*print_bll r;*) r  in
 let r =  (gen n) in 
   print_string "nb sols ";  print_int (list_length r); print_newline();;

queens 8 ;;

print_string "CORRECT-OK";;
print_newline();;

