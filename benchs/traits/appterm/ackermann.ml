let rec ack m n = 
  if m = 0 then n + 1 else
  if n = 0 then ack (m-1) 1 else
  ack (m-1) (ack m (n-1))
in
  print_int (ack 3 2) 

(* ~> 29 

ok sur l'implantation ocaml de la zam.
ko en miniML (explose la pile du simulateur nand2tetris)

*)