
let rec fib a b n =
 if n <= 1 then a
 else fib b (a + b) (n - 1)
in 
 print_int (fib 1 2 20)

 (* ~> 89 *)