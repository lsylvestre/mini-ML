
let _ = 
  assert (42 = 42);
  assert (let a = [|1;1|] in a.(0) = a.(1))

(* fonctions ----------------------- *)

let _ = assert (((fun x -> fun y -> x + y) 42 1) = 43)

let _ = 
  let a = 42 in
  let b = 24 in
  let c = 18 in
 (* print_int ((fun x -> a) 4) *)
   assert (((fun x -> a) 4) = 42);
   assert (((fun x -> a + b + c) 4) = 84);
   assert (((fun x -> a + b + c + x) 4) = 88);
   assert ((# (fun x -> a + b)).(1) = a);
   assert ((# (fun x -> a + b)).(2) = b);
   assert ((Array.length (# (fun x -> a + b))) = 3)

let _ = let a = 42 in
        assert (((fun x -> fun y -> x + y + a) 42 1) = 85)

(* fonction local, dans une fonction, mais pas encore dans une expression *)

let test1 () =           
	let f = fun x -> fun y -> x + y in
	assert (f 42 1 = 43)

let _ = test1 ()

let test2 () = 
  let a = 42 in
  let f = fun x -> x + a in
  assert (f 1 = 43)

let _ = test2 ()

(* types sommes ------------------------- *)

(* 1 constructeur constant *)

type t1 = T 

let _ = assert (# T = 0);    (* (#) ~ Obj.magic *)
        let x = T in
        let y = match x with | T -> 42 in 
        assert (y = 42)

(* plusieurs constructeurs constants *)

type t2 = A | B | C

let _ = 
  assert (# A = 0 && # B = 1 && # C = 2);
  let x = B in
  let y = match x with | C -> 0 | A -> 0 | B -> 42 in 
  assert (y = 42)

(* 1 constructeur paramétré *)

type cc = CC of (int)

let _ = 
  assert (# CC = 0);
  let x = CC 42 in
  let y = match x with CC(x) -> x in    (* attention typage, à revoir : si CC pas défini, exception Not_found *)
  assert (y = 42)

(* type option *)

type opt = None | Some of (int) 

let _ = 
  assert (# None = 0);
  assert (# Some = 1);
  let x = None in
  let y = match x with | None -> 17 | Some(x) -> x in 
  assert (y = 17)

let _ = 
  let x = Some 42 in
  let y = match x with | None -> 17 | Some(x) -> x + 1 in 
  assert (y = 43)

(* 1 constructeur avec plusieurs paramètres *)

type cc = App of (int * int * int)

let _ = 
  assert (# App = 0);
  let x1 = App (42,43,44) in
  let x2 = App 42 43 44 in    (* deux notations pour l'application de constructeurs *)
  let () = match x1 with 
          | App(a,b,c) -> assert (a = 42 && b = 43 && c = 44) in
  let () = match x2 with 
          | App(a,b,c) -> assert (a = 42 && b = 43 && c = 44) in
  assert (# x1 <> # x2) (* égalité de pointeur (==) *)  

(* NB : on ne peut pas filtrer le résultat de l'application partielle d'un constructeur *)
let marche_pas () = (* à revoir *)
  let x = App in
  let y = x 42 in
  let z = y 43 in 
  let () = match z 44 with
           | App(a,b,c) -> assert (a = 42 && b = 43 && c = 44) in
  let () = match z 45 with
           | App(a,b,c) -> assert (a = 42 && b = 43 && c = 45) in
  ()
 

(* type list *)

type lst = Nil | Cons of (int * lst) 

let _ = 
  assert (# Nil = 0);
  assert (# Cons = 1);
  let x = Nil in
  let y = match x with | Nil -> 17 | Cons(x,y) -> 42 in 
  assert (y = 17)

let _ = 
  let x = Cons (42, Cons(43, Cons (44,Nil))) in
  match x with 
          | Nil -> assert false
          | Cons(x1,t1) -> 
            assert (x1 = 42);
            (match t1 with 
             | Nil -> assert false
             | Cons(x2,t2) -> 
               assert (x2 = 43);
               (match t2 with 
               | Nil -> assert false
               | Cons(x3,t3) -> 
                 assert (x3 = 44);
                 (match t3 with 
                  | Nil -> assert true
                  | Cons(x3,t2) -> assert false)))

let rec sum x =
	if x < - 0 then 0 else sum (x - 1)

let _ = sum 30000
let _ = 
  print_string "ok"