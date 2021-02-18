(**************************************************************************)
(*                                                                        *)
(*         PSTL : OCaml sur la plate-forme Nand2Tetris (2020)             *)
(*                                                                        *)      
(*           Loïc SYLVESTRE              Pablito BELLO                    *)
(*           loic.sylvestre@etu.upmc.fr  pablito.bello@etu.upmc.fr        *)
(*                                                                        *)  
(**************************************************************************)


(*  c'est un triplet (mod_name * bc_body * init) *)
type bcmodule = Module of (Ast.name * instr list * Ast.name list)

and instrs = instr list          
and instr =
  | Comment of (string)
  | Push of (segment)
  | Pop of (segment)
  | Label of (label)
  | IfGoto of (label)
  | Goto of (label)
  | Return
  | Function of (fun_name * int) (* nb locales *)
  | Call of (fun_name * int) (* arité *)
  | BinOp of (vm_binop)
  | UnOp of (vm_unop)
  | False
  | True
and segment =
  | Anywhere
  | Argument of (int)
  | Constant of (int)
  | Static of (int)
  | Local of (int)
  | That of (int)
  | Temp of (int)
  | Pointer of (int)
             
and label = string
and fun_name = string

and vm_binop =
  Add | Sub | Mult | Div | Eq
  | Gt | Lt | And | Or | Assign
and vm_unop = Not | Access | Alloc
                               
