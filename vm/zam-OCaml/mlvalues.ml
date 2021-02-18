(**************************************************************************)
(*                                                                        *)
(*         PSTL : OCaml sur la plate-forme Nand2Tetris (2020)             *)
(*                                                                        *)      
(*           Loïc SYLVESTRE              Pablito BELLO                    *)
(*           loic.sylvestre@etu.upmc.fr  pablito.bello@etu.upmc.fr        *)
(*                                                                        *)  
(**************************************************************************)

(* version de mlvalue.ml pour l'implantation de la zam en OCaml, 
   qui distingue les entiers et les pointeurs par deux constructeurs dédiés,
   cela garantit par typage qu'on ne confond pas immédiat et pointeur quelque part. *)

type value = Long of long | Ptr of ptr
and long = int
and ptr = int

(* transforme un entier en mlvalue *)
let val_long (n : long) : value = Long n

(* transforme un mlvalue en entier *)
let long_val (v : value) : long = 
  match v with
  | Long n -> n
  | Ptr n -> n (* cast *)

(* transforme un pointeur en mlvalue *)
let val_ptr (p : ptr) : value = Ptr p


(* transforme un mlvalue en pointeur *)
let ptr_val (v : value) : ptr =
  match v with
  | Long n -> n (* cast *)
  | Ptr p -> p

let is_ptr (v : value) : bool = 
  match v with
  | Long _ -> false
  | Ptr _ -> true


let free (ptr : 'a array) = () 
