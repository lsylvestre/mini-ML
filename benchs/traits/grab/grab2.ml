let a = 42 in
let b = 38 in
let c = 4 in
let f = (fun x -> let w = 16 in 
                  fun y -> 
                    fun z -> 
                      x + y + a + b + w + z) in 
let g = f c in
let h = g 100 in
  print_int (h 40)

(* ~> 240 *)