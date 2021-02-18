
let l = 1 :: 2 :: 3 :: 4 :: 5 :: []

let rec iter f = function
| [] -> () 
| h::t-> let () = f h in iter f t ;;

iter print_int l;;

(* ~> 12345 *)