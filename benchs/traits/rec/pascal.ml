let rec triangle n p =
 if n = 0 || p = n then 1
 else triangle (n - 1) (p - 1) + triangle (n - 1) p
in
  print_int (triangle 6 10) 

(* ~> 64 *)