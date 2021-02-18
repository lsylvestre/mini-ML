(**************************************************************************)
(*                                                                        *)
(*         PSTL : OCaml sur la plate-forme Nand2Tetris (2020)             *)
(*                                                                        *)      
(*           Loïc SYLVESTRE              Pablito BELLO                    *)
(*           loic.sylvestre@etu.upmc.fr  pablito.bello@etu.upmc.fr        *)
(*                                                                        *)  
(**************************************************************************)

type prog = instr list          
and instr =
  | Push of (segment)
  | Pop of (segment)
  | Label of (string)
  | IfGoto of (string)
  | Goto of (string)
  | Function of ((string * string) * int) (* nb locales *)
  | Return
  | Call of ((string * string) * int) (* arité *)
  | Op of (op)
and segment =
  | Static of (int)
  | Temp of (int)
  | Argument of (int)
  | Constant of (int)
  | Local of (int)
  | This of (int)
  | That of (int)
and op = Add | Sub | Eq | Not | Lt | Gt | Land | Lor | Exit

