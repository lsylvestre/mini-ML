(**************************************************************************)
(*                                                                        *)
(*         PSTL : OCaml sur la plate-forme Nand2Tetris (2020)             *)
(*                                                                        *)      
(*           Loïc SYLVESTRE              Pablito BELLO                    *)
(*           loic.sylvestre@etu.upmc.fr  pablito.bello@etu.upmc.fr        *)
(*                                                                        *)  
(**************************************************************************)


open Printf
open Lexing

type pos = { start_pos: Lexing.position; end_pos: Lexing.position }

let make_position startp endp = { start_pos = startp; end_pos = endp } 

let default_position =  { start_pos = Lexing.dummy_pos; end_pos = Lexing.dummy_pos }

exception Parse_Exception of (string  * pos )


let string_of_position {start_pos={pos_lnum=l1;pos_cnum=c1};
                        end_pos={pos_lnum=l2;pos_cnum=c2}} =
  if l1 = l2 then sprintf "line %d, characters %d-%d" l1 c1 c2
  else sprintf "from line %d, characters %d, to line %d characters %d" l1 c1 l2 c2


let pos () = 
  make_position (Parsing.symbol_start_pos ()) (Parsing.symbol_end_pos ())

let error_exit pos msg =
  Printf.printf "error at : %s\n%s\n\nexit." (string_of_position pos) msg; exit 1
