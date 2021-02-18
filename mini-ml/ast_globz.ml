(**************************************************************************)
(*                                                                        *)
(*         PSTL : OCaml sur la plate-forme Nand2Tetris (2020)             *)
(*                                                                        *)      
(*           Loïc SYLVESTRE              Pablito BELLO                    *)
(*           loic.sylvestre@etu.upmc.fr  pablito.bello@etu.upmc.fr        *)
(*                                                                        *)  
(**************************************************************************)

(* extrait dans des variables globales les chaînes du programme, 
   pour éviter qu'elles ne soient allouées plusieurs fois *)

let gen = Gensym.create 0

let collect = ref []

(* globalise les chaînes de caractères dans le module mdl *)  

let rw_constant c = 
  match c with
  | Ast.String (_) -> 
     let k = Gensym.next "__static" gen in
     collect := (Ast.DefVar(k,Ast.Constant(c))):: !collect;
     Ast.Ident(k)
  | _ -> Ast.Constant(c)

let rec rw_exp e = 
  match e with
  | Ast.Constant(c) -> rw_constant c
  | Ast.Let(v,e1,e2) -> Ast.Let(v,rw_exp e1,rw_exp e2)
  | Ast.LetRec(v,e1,e2) -> Ast.LetRec(v,rw_exp e1,rw_exp e2)
  | Ast.App(e,args) -> Ast.App(rw_exp e,List.map rw_exp args) 
  | Ast.If(e1,e2,e3) -> Ast.If(rw_exp e1,rw_exp e2,rw_exp e3)
  | Ast.BinOp(op,e1,e2) -> Ast.BinOp(op,rw_exp e1,rw_exp e2)
  | Ast.UnOp(op,e1) -> Ast.UnOp(op,rw_exp e1)
  | Ast.Block(xs) -> Ast.Block(List.map rw_exp xs)
  | Ast.Seq(e1,e2) ->
     if (match e1 with
         | Ast.Constant(c) ->
            (match c with
               Ast.Unit -> true
             | _ -> false) 
         (* attention : on suppose que rien d'alloué ne vaut 0 *)
         | _ -> false )
     then rw_exp e2
     else Ast.Seq(rw_exp e1,rw_exp e2)
  | Ast.While(e1,e2) -> Ast.While(rw_exp e1,rw_exp e2)
  | Ast.Match(e,ms) ->
     let ms = List.map (fun m -> 
                  match m with
                  | Ast.Case(c,e) -> Ast.Case(c,rw_exp e) 
                  | Ast.Otherwise(e) -> Ast.Otherwise(rw_exp e)) ms in
     Ast.Match (rw_exp e, ms)
  | _ -> e

let rw_fundecs l = 
  List.map (fun d -> match d with
                     | Ast.DF (name,args,e) -> Ast.DF (name,args,rw_exp e)) l
let rec rw_decl d = 
  match d with
  | Ast.DefVar(v,e) -> Ast.DefVar(v,rw_exp e)
  | Ast.DefFun(l) -> Ast.DefFun (rw_fundecs l)
  | Ast.DefFunRec(l) -> Ast.DefFunRec (rw_fundecs l)
  | _ -> d 

let rewrite m =
  match m with 
  | Ast.Module(mod_name,decls) ->
     collect := [];
     let decls =
       List.rev_append  (* le reverse est important ! *)
         (!collect)
         (List.map rw_decl decls)
     in
     Ast.Module(mod_name,decls)
       

