(**************************************************************************)
(*                                                                        *)
(*         PSTL : OCaml sur la plate-forme Nand2Tetris (2020)             *)
(*                                                                        *)      
(*           Loïc SYLVESTRE              Pablito BELLO                    *)
(*           loic.sylvestre@etu.upmc.fr  pablito.bello@etu.upmc.fr        *)
(*                                                                        *)  
(**************************************************************************)

type prog = instr array          
and instr =
  | Push of (segment)
  | Pop of (segment)
  | IfGoto of (int)
  | Goto of (int)
  | Return
  | Call of (int * int * int) (* adresse * nb locales * arité *)
  | Op of (op)
  | Prim of (prim)
and segment =
  | Static of (int * int) (* numéro de fichier X index *)
  | Temp of (int)
  | Argument of (int)
  | Constant of (int)
  | Local of (int)
  | This of (int)
  | That of (int)
and op = Add | Sub | Eq | Not | Lt | Gt | Land | Lor | Exit

and prim = ML_exit | ML_array_length | ML_array_get | ML_array_set 
           | ML_array_create_uninitialized | ML_array_make | ML_print_int 
           | ML_print_char | ML_print_newline | ML_print_char_array | ML_make_pair
           | ML_left | ML_right | ML_obj_magic

let string_of_op = function  
  | Add -> "add" 
  | Sub -> "sub"
  | Eq -> "eq" 
  | Not -> "not"
  | Lt -> "lt" 
  | Gt -> "gt"
  | Land -> "land"
  | Lor -> "lor"
  | Exit -> "exit"

let string_of_prim = function
  | ML_exit -> "ML_exit"
  | ML_array_length -> "ML_array_length" 
  | ML_array_get -> "ML_array_get" 
  | ML_array_set  -> "ML_array_set"
  | ML_array_create_uninitialized -> "ML_array_create_uninitialized" 
  | ML_array_make -> "ML_array_make" 
  | ML_print_int -> "ML_print_int"
  | ML_print_char -> "ML_print_char"
  | ML_print_newline -> "ML_print_newline"
  | ML_print_char_array -> "ML_print_char_array" 
  | ML_make_pair -> "ML_make_pair" 
  | ML_left -> "ML_left"
  | ML_right -> "ML_right"
  | ML_obj_magic -> "ML_obj_magic"

let string_of_segment = function
  | Static (_,n) -> Printf.sprintf "static %d" n
  | Temp  n -> Printf.sprintf "temp %d" n
  | Argument n -> Printf.sprintf "argument %d" n
  | Constant n -> Printf.sprintf "constant %d" n
  | Local n -> Printf.sprintf "local %d" n
  | This n -> Printf.sprintf "this %d" n
  | That n -> Printf.sprintf "that %d" n

let string_of_instr = function
  | Push (seg) -> "push " ^ string_of_segment seg
  | Pop (seg) -> "pop " ^ string_of_segment seg
  | IfGoto (n) -> "ifgoto " ^ string_of_int n
  | Goto (n) -> "goto " ^ string_of_int n
  | Return -> "return"
  | Call(a,b,c) -> Printf.sprintf "call %d %d %d" a b c
  | Op op -> string_of_op op
  | Prim p -> "prim " ^ string_of_prim p
