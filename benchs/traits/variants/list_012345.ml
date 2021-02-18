let l = 0 :: 1 :: 2 :: 3 :: 4 :: 5 :: [] ;;

match l with
| [] -> ()
| h0::h1::h2::h3::t -> print_int h0 ;
                       print_int h1 ; 
                       print_int h2 ; 
                       print_int h3 ;;