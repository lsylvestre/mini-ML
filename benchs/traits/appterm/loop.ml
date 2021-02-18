
let rec f = function 
| 0 -> 42
| n -> f (n - 1) 
 
 in print_int (f 6)