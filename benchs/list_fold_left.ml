

let rec fold_left f accu l =
  match l with
    [] -> accu
  | a::l -> fold_left f (f accu a) l ;;



print_int (fold_left (+) 0 [1;2;3;4;5;6;7;8;9;10]) ;;
