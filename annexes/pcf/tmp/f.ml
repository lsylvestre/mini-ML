letrec f = \f.f in f 2.


(* letrec mult = \x.\y.ifz x then 0 else y+mult(x-1) y in
letrec fact = \x.in ifz x then 1 else mult x (fact (x-1)) 
in fact 6. *)

(*let a = 17 in
let b= 5 in
(\x.\y.\z.\u.((\v.(v+a))x+y+b)) 5 6 7 8. *)
(*letrec sum = 
  \x.\z.ifz x then 1 else x +sum (x-1) z 
in (sum 6 42). *)