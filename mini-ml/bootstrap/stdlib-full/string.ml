(* bibliotheque d'exécution pour mini-ml *)
(* module String                         *)                  
(*                                       *)
(* Loïc Sylvestre                        *)

let make (n : int) (c : char) : string = 
  Internal.obj_magic (Internal.array_make n (Internal.obj_magic c))

let get (s : string) (i : int) : char = 
	Internal.array_get (Internal.obj_magic s) i

let length (s : string) : int = 
	Internal.array_length (Internal.obj_magic s)


let rec aux_equal (s1 : string) (s2 : string) n =
  if n < 0 
  then true 
  else if (# (get s1 n)) = (# (get s2 n))
       then aux_equal s1 s2 (n-1) 
       else false

let equal (s1 : string) (s2 : string) : bool =
	let l1 = length s1 in
	aux_equal s1 s2 (l1-1)

let iter f s =
  for i = 0 to length s - 1 do f (get s i) done


let rec index_rec_opt s lim i c =
  if i >= lim then None else
  if # (get s i) = #c then Some i else index_rec_opt s lim (i + 1) c

let index_opt s c = index_rec_opt s (length s) 0 c
