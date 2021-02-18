(**************************************************************************)
(*                                                                        *)
(*         PSTL : OCaml sur la plate-forme Nand2Tetris (2020)             *)
(*                                                                        *)      
(*           Loïc SYLVESTRE              Pablito BELLO                    *)
(*           loic.sylvestre@etu.upmc.fr  pablito.bello@etu.upmc.fr        *)
(*                                                                        *)  
(**************************************************************************)

type loc = Parseutils.pos

type prog = tmodule list

and tmodule = {
    mod_name : name ; 
    decls : decl list
  }
            
and name = string
and var = (name * typ option)

and decl = {
    decl_desc: declaration_desc; 
    decl_loc : loc
  }
         
and declaration_desc =
  | DefVar of (var * exp)
  | DefFun of    ((name * var list * typ option * exp) list)
  | DefFunRec of ((name * var list * typ option * exp) list)
  | Type of (name * name list * ty)
and ty = Exp_ty of typ | Sum of constr list
and constr = name * typ list
and exp = { exp_desc: expression_desc; 
            exp_loc : loc }
and expression_desc =
  | Annotation of (exp * typ)
  | Constant of (constant)
  | Ident of (name)
  | Let of (var * exp * exp)
  | LetRec of (name * exp * exp)
  | Fun of (var * exp)
  (* | LetFun of (name * name list * exp * exp) *)
  | App of (exp * exp list)
  | If of (exp * exp * exp)
  | Match of (exp * match_case list)
  | BinOp of (binop * exp * exp)
  | UnOp of (unop * exp)
  | Pair of (exp * exp)
  | Array_create of (exp list)
  | Array_assign of (exp * exp * exp)
  | Array_access of (exp * exp)
  | Ref of (exp)
  | Ref_access of (exp)
  | Ref_assign of (exp * exp)
  | Seq of (exp * exp)
  | While of (exp * exp)
  | For of (name * exp * exp * exp)
  | Magic of (exp)
  | Assert of (exp * Parseutils.pos)
and constant = 
  | Unit
  | Bool of bool
  | Int of int
  | Char of char
  | String of string
  | Constr of string
  | Array_empty
and match_case =
  | Case of (constant * name list * exp)  (* eg. [A(x,y,z) -> e] *)
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

and typ =
  | Tint
  | Tbool
  | Tunit
  | Tchar
  | Tstring
  | Tvar of name
  | Tarrow of typ * typ
  | Tproduct of typ * typ
  | Tarray of typ
  | Tref of typ
  | Tconstr of (name * typ list)
  | Tident of name
