(**************************************************************************)
(*                                                                        *)
(*         PSTL : OCaml sur la plate-forme Nand2Tetris (2020)             *)
(*                                                                        *)      
(*           Loïc SYLVESTRE              Pablito BELLO                    *)
(*           loic.sylvestre@etu.upmc.fr  pablito.bello@etu.upmc.fr        *)
(*                                                                        *)  
(**************************************************************************)


type prog = tmodule list

(* Le type OCaml initial était :    
and tmodule = {
    mod_name : Ast.name ;
    decls: decl list ;
    init : Ast.name list
  } 
*)

and tmodule = Module of (Ast.name * decl list * Ast.name list)

and decl = 
  | DefFun of (Ast.name * arity * exp)
and arity = int
and exp = 
  | Constant of constant
  | Variable of var
  | GFun of (Ast.name)
  | Closure of ((int * exp) * exp) (* [code,env]  avec code = {adr | e } *)
  | Let of (int * exp * exp)
  | App of (exp * exp list)
  | If of (exp * exp * exp)
  | BinOp of (Ast.binop * exp * exp)
  | UnOp of (Ast.unop * exp)
  | Seq of (exp * exp)
  | While of (exp * exp)
  | Ext of ext
and constant = 
  | Unit 
  | Bool of bool 
  | Int of int 
  | Array_empty
(* | FunValue of int *)
and var = 
  | Global of string
  | Argument of int 
  | Local of int
  | Free of int
and ext =   
  | SetGlobal of (exp * int)
  | ReadGlobal of (int)
  | SetLocal of (int * exp)
  | Goto of (string * exp list)
  | Label of (string * exp)
