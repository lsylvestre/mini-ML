(**************************************************************************)
(*                                                                        *)
(*         PSTL : OCaml sur la plate-forme Nand2Tetris (2020)             *)
(*                                                                        *)      
(*           Loïc SYLVESTRE              Pablito BELLO                    *)
(*           loic.sylvestre@etu.upmc.fr  pablito.bello@etu.upmc.fr        *)
(*                                                                        *)  
(**************************************************************************)


let labels = ref []
let add_label l pc = 
  labels := (l,pc) :: !labels

let functions = ref []
let add_function l pc nblocals = 
  functions := (l,(pc,nblocals)) :: !functions

let instr pc = function
  | Bc.Label l -> add_label l pc; pc
  | Bc.Function ((m,f),n) -> add_function (m,f) pc n; pc
  | _ -> pc + 1

let prog l = (* i est le numéro du fichier *)
  let rec aux pc = function
    | [] -> (!labels,!functions)
    | ins::r -> let pc' = instr pc ins in
                aux pc' r
  in
  aux 0 l
    
