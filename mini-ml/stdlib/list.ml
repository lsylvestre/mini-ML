(* bibliotheque d'exécution pour mini-ml *)
(* module List                           *)                  
(*                                       *)
(* Loïc Sylvestre                        *)

let cons x l = x :: l

let hd l = 
  match l with 
  | [] -> failwith "hd"
  | x::_ -> x

let tl l = 
  match l with 
  | [] -> failwith "tl"
  | _::t -> t
  
let rec map f l =
   match l with
   | [] -> []
   | x::t -> f x :: (map f t)


let rec rev_append l1 l2 =
  match l1 with
  | [] -> l2
  | x::r -> rev_append r (x :: l2)

let rev l = rev_append l []

let append l1 l2 = rev_append (rev l1) l2

let rec concat ls = 
 match ls with  
 | [] -> []
 | l::r -> append l (concat r)
