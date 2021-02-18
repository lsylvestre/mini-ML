(* bibliotheque d'exécution pour mini-ml *)
(* module Array                          *)                  
(*                                       *)
(* Loïc Sylvestre                        *)

let make (n : int) (x : 'a) : 'a array = Internal.array_make n x
let create n x = make n x
let create_uninitialized (n : int) : 'a array = Internal.array_create_uninitialized n
let get (a : 'a array) (i : int) : 'a = Internal.array_get a i
let set (a : 'a array) (i : int) (x : 'a) : unit = Internal.array_set a i x
let length (a : 'a array) : int = Internal.array_length a

let iter (f : 'a -> unit) (a : 'a array) : unit =
  for i = 0 to length a - 1 do f (get a i) done 

let map (f : 'a -> 'b) (a : 'a array) : 'b array = 
	let n = length a in
	if n = 0 then [||] 
    else let a2 = make n (get a 0) in
           for i = 0 to n - 1 do
   	 set a2 i (f (get a i))
  done;
  a2
