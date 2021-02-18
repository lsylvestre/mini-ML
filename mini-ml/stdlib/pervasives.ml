(* bibliotheque d'exécution pour mini-ml *)
(* module Pervasives                     *)                  
(*                                       *)
(* Loïc Sylvestre                        *)

let exit (n : int) : unit = 
	Internal.exit n

let ref x = 
   ref_ x (* Internal.array_make 1 x *)

let ref_contents r = 
	!r

let ref_set_contents r x = 
	r := x

let incr r = 
	r := (!r + 1)

let decr r = 
  r := (!r - 1)

let not b = if b then false else true

let fst (p : 'a * 'b) : 'a = 
	(# p).(0)

let snd (p : 'a * 'b) : 'b = 
	(# p).(1)

let print_int (n : int) : unit = 
	Internal.print_int n

let print_char (c : char) : unit = 
	Internal.print_char c
  
let print_newline (u : unit) : unit = 
	Internal.print_newline u

let print_string (s : string) : unit = 
	Internal.print_char_array s

let failwith (msg : string) : unit = 
	print_string msg; 
    print_newline (); 
    exit 1

let ignore a = (Obj.magic a)

let abs (n : int) : int = if n >= 0 then n else - n

let max a b = if a > b then a else b
 
let (^) s1 s2 = 
  let l1 = Internal.array_length s1 in
  let l2 = Internal.array_length s2 in
  let s = Internal.array_create_uninitialized (l1+l2) in
  for i = 0 to l1 - 1 do
  	Internal.array_set s i (Internal.array_get s1 i)
  done;
  for i = 0 to l2 - 1 do
  	Internal.array_set s (i+l1) (Internal.array_get s2 i)
  done;
  s

let copy s = 
  let l = Internal.array_length s in
  let r = Internal.array_create_uninitialized l in
  for i = 0 to l - 1 do
    Internal.array_set r i (Internal.array_get s i)
  done;
  r


let rec aux_equal t1 t2 n =
  if n < 0 
  then true 
  else if (# (Internal.array_get t1 n)) = (# (Internal.array_get t2 n))
       then aux_equal t1 t2 (n-1) 
       else false

let equal t1 t2 : bool =
  let l1 = Internal.array_length t1 in
  aux_equal t1 t2 (l1-1)


let string_of_int n = Internal.array_make 1 n
let int_of_char (n : char) : int = (# n)

let compare x y = if x < y then -1 else if x = y then 0 else 1


let output_char oc c = print_char c 