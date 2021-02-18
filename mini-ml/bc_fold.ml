(**************************************************************************)
(*                                                                        *)
(*         PSTL : OCaml sur la plate-forme Nand2Tetris (2020)             *)
(*                                                                        *)      
(*           Loïc SYLVESTRE              Pablito BELLO                    *)
(*           loic.sylvestre@etu.upmc.fr  pablito.bello@etu.upmc.fr        *)
(*                                                                        *)  
(**************************************************************************)

(* attention, la propagation des Bc.Constantes
   devrait respecter les spécificité de l'architecture Hack (addition 16 bits etc.) *)

let bc_int n = [Bc.Push (Bc.Constant(n))]

let rec rw bc = 
  match bc with
  | [] -> []
  | Bc.Push(Bc.Constant n) :: Bc.Push(Bc.Constant m) :: Bc.BinOp(Bc.Add) :: bc -> 
     rw @@ (bc_int ((n + m) mod 0xFFFF)) @ bc
  | Bc.Push(Bc.Constant n) :: Bc.Push(Bc.Constant m) :: Bc.BinOp(Bc.Sub) :: bc -> 
     rw @@ (bc_int ((n - m) mod 0xFFFF)) @ bc
  | Bc.Push(Bc.Constant n) :: Bc.Push(Bc.Constant m) :: Bc.BinOp(Bc.Mult) :: bc -> 
     rw @@ (bc_int ((n * m) mod 0xFFFF)) @ bc
  | Bc.Push(Bc.Constant n) :: Bc.Push(Bc.Constant m) :: Bc.BinOp(Bc.Div) :: bc -> 
     rw @@ (bc_int ((n / m) mod 0xFFFF)) @ bc
  | Bc.Push(Bc.Constant n) :: Bc.Push(Bc.Constant m) :: Bc.BinOp(Bc.Eq) :: bc -> 
     rw @@ (if n = m then Bc.True else Bc.False) :: bc
  | Bc.Push(Bc.Constant n) :: Bc.Push(Bc.Constant m) :: Bc.BinOp(Bc.Gt) :: bc -> 
     rw @@ (if n > m then Bc.True else Bc.False) :: bc
  | Bc.Push(Bc.Constant n) :: Bc.Push(Bc.Constant m) :: Bc.BinOp(Bc.Lt) :: bc -> 
     rw @@ (if n < m then Bc.True else Bc.False) :: bc
  | Bc.False :: Bc.False :: Bc.BinOp(Bc.And) :: bc
    | Bc.False :: Bc.True :: Bc.BinOp(Bc.And) :: bc
    | Bc.True :: Bc.False :: Bc.BinOp(Bc.And) :: bc -> rw (Bc.False :: bc)
  | Bc.True :: Bc.True :: Bc.BinOp(Bc.And) :: bc -> rw (Bc.True :: bc)
  | Bc.False :: Bc.False :: Bc.BinOp(Bc.Or) :: bc -> rw (Bc.False :: bc)
  | Bc.False :: Bc.True :: Bc.BinOp(Bc.Or) :: bc
    | Bc.True :: Bc.False :: Bc.BinOp(Bc.Or) :: bc 
    | Bc.True :: Bc.True :: Bc.BinOp(Bc.Or) :: bc -> rw (Bc.True :: bc)
  | s::bc -> s::(rw bc)
                  (* | Goto l1 :: Goto l2 :: bc ->  *)


let rewrite bcm = 
  match bcm with 
  | Bc.Module (mod_name,bc_body,init) -> 
    let bc_body=rw bc_body in
    Bc.Module (mod_name,bc_body,init)
