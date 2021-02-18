type 'a opt = S of 'a | N ;;

let f x  = S x ;;

match f 42 with
| N -> ()
| S x -> print_int x ;;

print_newline () ;;