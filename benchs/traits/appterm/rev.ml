
let rec iter f = function
| [] -> ()
| h::t -> f h; iter f t ;;

let rec rev acc = function
| [] -> acc
| h::t -> rev (h::acc) t ;;


iter print_int [1;2;3;4;5;6;7;8];;



iter print_int (rev [] [1;2;3;4;5;6;7;8]);;

