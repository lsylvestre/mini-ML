(**************************************************************************)
(*                                                                        *)
(*         PSTL : OCaml sur la plate-forme Nand2Tetris (2020)             *)
(*                                                                        *)      
(*           Loïc SYLVESTRE              Pablito BELLO                    *)
(*           loic.sylvestre@etu.upmc.fr  pablito.bello@etu.upmc.fr        *)
(*                                                                        *)  
(**************************************************************************)

(* Hypothèse :
   Les segments mémoires sont placés dans cet ordre :
   [| DATA |] [| GLOBALES |] [| SEMI-SPACE1 |] [| SEMI-SPACE2  |] 
 *)

(* taille de chaque segment *)

let data_size = 1024
let global_size = 1024
let heap_size = ref 4096 (* par semi-space *)
let stack_size = 2048

(* segment data *)
(* commence à l'adresse 0 *)
let data_top = ref 0
let data = Array.make data_size (Mlvalues.val_long 0)

(* globales *)
let global_start = data_size
let global_top = ref 0
let global = Array.make global_size (Mlvalues.val_long 0)

(* tas *)
let heap_start = global_start+global_size
let from_space = ref (Array.make !heap_size (Mlvalues.val_long 0))
let to_space = ref (Array.make !heap_size (Mlvalues.val_long 0))
let heap_top = ref 0

(* pile *)
let sp = ref 0
let stack = Array.make stack_size (Mlvalues.val_long 0)

(* registres *)
let acc = ref (Mlvalues.val_long 0)
let env = ref (Mlvalues.val_long 0)
