let a = 42 in
let b = 38 in
let c = 4 in
let f x y = x + y + a + b in 
let g = f c in
  print_int (g 100)


(* ~> 184 *)