let a = 42 in
let b = 17 in
let c = 25 in
let d = 8 in
let e = 3 in
let rec f1 x = a + (f4 x)
and f2 x = b + (f3 x)
and f3 x = if x = 0 then c else a + (f1 (x-1))
and f4 x = d + e + (f2 x)
in 
  print_int (f3 3) ;;
