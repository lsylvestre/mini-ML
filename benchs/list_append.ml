
let l1 = [1;2;3;4;5]
let l2 = [6;7;8;9;10]

let rec iter f = function
  | [] -> () 
  | h::t-> let () = f h in iter f t ;;

let rec append l1 l2 = match l1 with 
    [] -> l2
  | a::q -> a::(append q l2) ;;

iter print_int (append l1 l2) ;;

(* ~> 12345678910 *)
