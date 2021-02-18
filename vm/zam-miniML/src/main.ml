(**************************************************************************)
(*                                                                        *)
(*         PSTL : OCaml sur la plate-forme Nand2Tetris (2020)             *)
(*                                                                        *)      
(*           Loïc SYLVESTRE              Pablito BELLO                    *)
(*           loic.sylvestre@etu.upmc.fr  pablito.bello@etu.upmc.fr        *)
(*                                                                        *)  
(**************************************************************************)

(* point d'entrée de notre implémentation de la ZAM, 
   interprète le tableau de bytecode Input.code *)

let print_end () =
  print_newline ();
  print_string "STOP.";
  print_newline ();
  Interp.debug_print_state ();
  print_newline ()

let _ = 
  Interp.interp Input.code;
  if Interp.debug then print_end ()
  


