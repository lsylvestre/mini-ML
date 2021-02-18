
let rec fact acc = function 
| 0 -> acc
| n -> fact (n * acc) (n - 1) 
 
 in print_int (fact 1 6)