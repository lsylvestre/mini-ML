let rec f l = 
  let a = 5 in
  let b = 6 in
  let g () = a + b in
  print_int 42; 
  f (g () :: [])
 in
 f []


(* ~> ne fonctionne pas. à revoir. *)