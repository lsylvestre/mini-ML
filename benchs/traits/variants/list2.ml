let l = 0 :: 1 :: 42 :: [] ;;

match l with
| [] -> ()
| h0::t -> print_int h0 

(* fonctionne en ocaml. Sur la ZAM en mini-ML, ça affiche 42, bizarrement ... *)