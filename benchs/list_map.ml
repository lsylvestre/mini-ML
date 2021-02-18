let rec iter f = function
| [] -> () 
| h::t-> let () = f h in iter f t ;;

let rec map f l =
  match l with
    [] -> []
  | a::l -> f a :: (map f l);;

iter print_int (map succ [1;2;3;4;5;6;7;8;9;10]);;
