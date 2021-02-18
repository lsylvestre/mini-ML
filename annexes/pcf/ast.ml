(**************************************************************************)
(*                                                                        *)
(*         PSTL : OCaml sur la plate-forme Nand2Tetris (2020)             *)
(*                                                                        *)      
(*           Loïc SYLVESTRE              Pablito BELLO                    *)
(*           loic.sylvestre@etu.upmc.fr  pablito.bello@etu.upmc.fr        *)
(*                                                                        *)  
(**************************************************************************)

type name = string
and exp = 
  | Var of (name)
  | Let of (name * exp * exp)
  | Letrec of (name * exp * exp)
  | Lam of (name * exp)
  | App of (exp * exp)
         
  | Int of int
  | Add of (exp * exp)
  | Sub of (exp * exp)
  | IfZero of (exp * exp * exp)


let rec string_exp e =
  match e with 
  | Var(x) -> x
  | Let (x,e1,e2) -> "(let " ^ x ^ " = "
                     ^ string_exp e1 ^ " in\n"
                     ^ string_exp e2 ^ ")"
  | Letrec (x,e1,e2) -> "(letrec "
                        ^ x
                        ^ " = "
                        ^ string_exp e1
                        ^ " in\n"
                        ^ string_exp e2 ^ ")"
  | Lam(x,e) -> "(\\" ^ x ^ "." ^ string_exp e ^ ")"
  | App(e1,e2) -> "(" ^ string_exp e1 ^ " " ^ string_exp e2 ^ ")"
  | Int n -> string_of_int n
  | Add (e1,e2) ->  "(" ^ string_exp e1 ^ " + " ^ string_exp e2 ^ ")"
  | Sub (e1,e2) ->  "(" ^ string_exp e1 ^ " - " ^ string_exp e2 ^ ")"
  | IfZero (e1,e2,e3) ->  "(ifz " ^ string_exp e1 ^ " then " ^
                            string_exp e2 ^ " else  " ^
                              string_exp e3 ^ ")"

let string_prog e = 
  string_exp e ^ "."
