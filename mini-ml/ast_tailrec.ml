(**************************************************************************)
(*                                                                        *)
(*         PSTL : OCaml sur la plate-forme Nand2Tetris (2020)             *)
(*                                                                        *)      
(*           Loïc SYLVESTRE              Pablito BELLO                    *)
(*           loic.sylvestre@etu.upmc.fr  pablito.bello@etu.upmc.fr        *)
(*                                                                        *)  
(**************************************************************************)

(* élimination des appels terminaux *)

exception Abort

let rec rewrite m =
  match m with Ast.Module(mod_name,decls) ->
    let decls = List.map rw_decl decls in
    Ast.Module(mod_name,decls)

and rw_decl d = match d with
  | Ast.DefFunRec l -> 
     Ast.DefFunRec (List.map (fun (Ast.DF (name,args,e) as f) -> 
                        (try let f' = rw_defun f in 
                            Ast.DF (name,args,f')
                         with Abort -> f)) l )
  | d -> d
and rw_defun (Ast.DF (name,args,e)) =
  Ast.Ext(Ast.Label(name,rw_exp name args e))
and rw_exp f args exp = 
  let rec rw_exp exp = match exp with 
    | Ast.Ident(name) -> if f = name then raise Abort else exp
    | Ast.Let(name,e1,e2) -> 
       if name = f then Ast.Let(name,rw_exp e1,e2)
       else Ast.Let(name,e1,rw_exp e2)
    | Ast.App(Ast.Ident name,argv) -> 
       if name = f 
       then (Ast.Ext(Ast.Goto(name,argv))) else exp
    | Ast.If(e1,e2,e3) ->
       Ast.If(e1,rw_exp e2,rw_exp e3)
    | Ast.Seq(e1,e2) -> Ast.Seq(e1,rw_exp e2)
    | Ast.Match (e,ms) ->
       let ms = List.map
                  (function 
                   | Ast.Case (c,e) -> Ast.Case(c,rw_exp e)
                   | Ast.Otherwise e -> Ast.Otherwise (rw_exp e)) ms in
       Ast.Match (e,ms)          
    | _ -> exp
  in
  rw_exp exp
