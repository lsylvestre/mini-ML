
type intList = Nil | Cons of (int * intList)

let rec map f = function
| Nil -> Nil
| Cons (x,l) -> Cons (f x,map f l) ;;

let rec iter f = function
| Nil -> Nil
| Cons (x,l) -> f x ; iter f l ;;


let l = Cons(1,Cons(2,Cons(3,Cons(4,Nil)))) ;;

let _ = iter print_int l ;;  


(* ~> assert failure ddans obytelib *)