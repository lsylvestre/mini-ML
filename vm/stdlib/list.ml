

let rec iter f = function
| [] -> () 
| h::t-> let () = f h in iter f t ;;

let rec rev acc = function
| [] -> acc
| h::t -> rev (h::acc) t ;;

let rec append l1 l2 = match l1 with 
  [] -> l2
| a::q -> a::(append q l2);;

let rec map f l =
  match l with
    [] -> []
  | a::l -> f a :: (map f l);;
