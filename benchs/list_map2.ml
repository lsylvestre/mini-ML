let rec iter f = function
  | [] -> () 
  | h::t-> let () = f h in iter f t ;;

let rec map2 f l1 l2 =
  match l1,l2 with
  | a1::l1,a2::l2 -> f a1 a2 :: (map2 f l1 l2)
  | _ -> [] ;;

iter print_int (map2 (+) [10;9;8;7;6;5;4;3;2;1] [1;2;3;4;5;6;7;8;9;10]);;
