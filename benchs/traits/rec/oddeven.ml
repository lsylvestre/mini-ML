
let rec even = function 0 -> true | n -> odd (n-1)
and odd = function 0 -> false | n -> even (n-1) in
if even 42 
then print_int 1 
else print_int 0