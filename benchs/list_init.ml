let sep = "," ;;

let print n = print_int n; print_string sep ;;

let rec iter f = function
    [] -> () 
  | h::t-> let () = f h in iter f t ;;

let init f n =
  let rec aux acc = function 
      0 -> acc
    | n -> aux (f n :: acc) (pred n) 
  in aux [] n

let rec fact = function 
    0 -> 1
  | n -> n * fact (pred n) ;;

iter print (init fact 10) ;;
