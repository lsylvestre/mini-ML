let a = "m4.a" ;;
let b = "m4.b" ;;
let c = "m4.c" ;;
let d = "m4.d" ;;
let e = M3.e ;;

print_string M3.c;;
print_newline ();; 
print_int 17;;
print_string c;;
print_newline ();; 

let n = 56 ;;

print_int n;;

match M3.e with 
| x1 :: _ -> print_int x1
| _ -> () ;; 