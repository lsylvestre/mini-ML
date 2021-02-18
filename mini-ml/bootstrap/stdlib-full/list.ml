(* bibliotheque d'exécution pour mini-ml *)
(* module List                           *)                  
(*                                       *)
(* Loïc Sylvestre                        *)

(* type 'a list = [] | (::) of ('a * 'a list) *)

let cons (x : 'a) (l : 'a list) : 'a list = (x::l)

let hd (l : 'a list) : 'a = 
 match l with
 | [] -> failwith "tl"
 | x::_ -> x  

let tl (l : 'a list) : 'a = 
 match l with
 | [] -> failwith "tl"
 | _::r -> r  

let rec map (f : 'a -> 'b) (l : 'a list) : 'b list =
   match l with
   | [] -> []
   | x::r ->  (f x :: (map f r))


let rec length_aux len l = match l with
    [] -> len
  | _::r -> length_aux (len + 1) r

let length l = length_aux 0 l


let rec iter f l =
  match l with 
  | [] -> ()
  | x::r -> f x; iter f r

let rec exists p l =
  match l with 
  | [] -> false
  | x::r -> if p x then true else exists p r 


let rec rev_append l1 l2 =
  match l1 with
  | [] -> l2
  | x::r -> rev_append r (x :: l2)

let rev l = rev_append l []

let append l1 l2 = rev_append (rev l1) l2


let rec fold_left f accu l =
  match l with
    [] -> accu
  | a::l -> fold_left f (f accu a) l


let rec fold_right f l accu =
  match l with
  | [] -> accu
  | a::r -> f a (fold_right f r accu)


let rec concat ls = 
 match ls with  
 | [] -> []
 | l::r -> append l (concat r)

let rec assoc_opt e l =
  match l with
  | [] -> Pervasives.None
  | x::l -> let a = fst x in
            let b = snd x in
            if String.equal e a then Pervasives.Some b else assoc_opt e l

let rec mapi_aux i f l = 
  match l with
  | [] -> []
  | a::tl -> let r = f i a in r :: mapi_aux (i + 1) f tl

let mapi f l = mapi_aux 0 f l 

let rec rmap_aux f accu l = 
  match l with  
    | [] -> accu
    | a::l -> rmap_aux f (f a :: accu) l

let rev_map f l =
  rmap_aux f [] l


let rec insert e l = 
match l  with 
  | [] -> e :: []
  | x :: l -> if (Obj.magic e) < (Obj.magic x) then e :: x :: l
              else x :: insert e l

let rec sort compare l = 
match l  with
  | [] -> []
  | x :: l -> insert x (sort l)

let rec memq x l =
  match l with
  | [] -> false
  | a::l -> (# a) = (# x) || memq x l

let rec mem x l =
  match l with
  | [] -> false
  | a::l -> String.equal (# a) (# x) || mem x l



let rec nth_aux l n =
    match l with
    | [] -> failwith "nth"
    | a::l -> if n = 0 then a else nth_aux l (n-1)

let nth l n =
  if n < 0 then failwith "List.nth" else
  nth_aux l n

let rec partition_aux p yes no l = match l with
  | [] -> (rev yes, rev no)
  | x :: l -> if p x then partition_aux p (x :: yes) no l else partition_aux p yes (x :: no) l

let partition p l =
  partition_aux p [] [] l
