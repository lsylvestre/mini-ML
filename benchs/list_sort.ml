
let rec iter f = function
  | [] -> ()
  | h::t -> f h; iter f t ;;

let rec sort = function
  | [] -> []
  | x :: l -> insert x (sort l)

and insert elem = function
  | [] -> [elem]
  | x :: l -> if elem < x then elem :: x :: l
              else x :: insert elem l ;;




iter print_int (sort [6;1;3;7;2;5;9;4]) ;;

