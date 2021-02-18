(**************************************************************************)
(*                                                                        *)
(*         PSTL : OCaml sur la plate-forme Nand2Tetris (2020)             *)
(*                                                                        *)      
(*           Loïc SYLVESTRE              Pablito BELLO                    *)
(*           loic.sylvestre@etu.upmc.fr  pablito.bello@etu.upmc.fr        *)
(*                                                                        *)  
(**************************************************************************)


(* en mini-ml *)

type ty = Past.ty
type pos = Parseutils.pos

type prog = tmodule list
and tmodule = Module of (name * decl list)
and name = string
and decl = 
  | DefVar of (name * exp)
  | DefFun of    (defun list)
  | DefFunRec of (defun list)
  | Sum of (name list)
and defun = DF of (name * name list * exp)
and exp = 
  | Constant of (constant)
  | Ident of (name)
  | Let of (name * exp * exp)
  | LetRec of (name * exp * exp)
  | Fun of (name * exp)
  | Closure of ((int * exp) * string * exp)  (* introduit plus tard *)
  | App of (exp * exp list)
  | If of (exp * exp * exp)
  | Match of (exp * match_case list)
  | BinOp of (binop * exp * exp)
  | UnOp of (unop * exp)
  | Block of (exp list)
  | Seq of (exp * exp)
  | While of (exp * exp)
  | Ext of ext
and constant = 
  | Unit
  | Bool of bool
  | Int of int
  | Char of char
  | String of string
  | Array_empty
and match_case =
  | Case of (constant * exp)
  | Otherwise of (exp) 

and binop =
  | Add
  | Minus
  | Mult
  | Div
  | Lt
  | Le
  | Neq
  | Eq
  | Ge
  | Gt
  | Or
  | And
  | Lor
  | Land

and unop =
  | UMinus

and ext = 
  | Array_alloc of (exp) 
  | SetGlobal of (exp * int)
  | ReadGlobal of (int)
  | Goto of (string * exp list)
  | Label of (string * exp)
