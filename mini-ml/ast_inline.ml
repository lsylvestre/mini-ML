(**************************************************************************)
(*                                                                        *)
(*         PSTL : OCaml sur la plate-forme Nand2Tetris (2020)             *)
(*                                                                        *)      
(*           Loïc SYLVESTRE              Pablito BELLO                    *)
(*           loic.sylvestre@etu.upmc.fr  pablito.bello@etu.upmc.fr        *)
(*                                                                        *)  
(**************************************************************************)

let rec depth e = match e with 
  | Ast.Constant c -> 0
  | Ast.Let(name,e1,e2) -> 1 + max (depth e1) (depth e2)
  | Ast.LetRec(name,e1,e2) -> 1 + max (depth e1) (depth e2)
  | Ast.App(e,args) -> 1 + max (depth e) (depth_list args)
  | Ast.If(e1,e2,e3) -> 1 + max (depth e1) (max (depth e2) (depth e3))
  | Ast.BinOp(op,e1,e2) -> 1 + max (depth e1) (depth e2)
  | Ast.UnOp(op,e1) -> 1 + depth e1 
  | Ast.Seq(e1,e2) -> 1 + max (depth e1) (depth e2)
  | Ast.While(e1,e2) -> 1 + max (depth e1) (depth e2)
  | Ast.Match(e,ms) -> 1 + max (depth e) (depth_list (List.map
                                                        (function
                                                           Ast.Case(_,e) -> e
                                                         | Ast.Otherwise e -> e) ms))
  | Ast.Ident name -> 0
  | e -> 0
and depth_list l = 
  List.fold_left (fun acc e -> max acc (depth e)) 0 l


let rec visit_tmodule ?(depth_max=5) m =
  if depth_max <= 0 then m else 
    match m with Ast.Module(mod_name,decls) ->
      let _,decls = visit_decls ~depth_max [] [] decls in
      Ast.Module(mod_name,decls)

and visit_decls ?(depth_max=10) env acc = function
  | [] -> (env,List.rev acc)
  | Ast.DefFun(l)::ds -> 
     let l = List.map
               (fun (Ast.DF (name,args,e)) ->
                 Ast.DF (name,args,visit_exp env e)) l in
     let env = List.rev @@
                 List.fold_left (fun env (Ast.DF (name,args,e)) -> 
                     if depth e < depth_max
                     then (name,(args,e)) :: env
                     else env) env l in
     visit_decls env (Ast.DefFun(l) :: acc) ds
  | Ast.DefVar(v,e)::ds -> visit_decls env (Ast.DefVar(v,visit_exp env e) :: acc) ds
  | Ast.DefFunRec(l)::ds -> 
     let l = List.map
               (fun (Ast.DF (name,args,e)) ->
                 Ast.DF (name,args,visit_exp env e)) l in
     visit_decls env (Ast.DefFunRec(l) :: acc) ds
  | d::ds ->  visit_decls env (d :: acc) ds
and visit_exp env e = 
  match e with
  | Ast.Ident name -> e
  | Ast.Constant c -> e
  | Ast.Let(v,e1,e2) -> (match List.assoc_opt v env with
                         | None -> Ast.Let(v,visit_exp env e1,visit_exp env e2)
                         | Some _ -> Ast.Let(v,visit_exp env e1,e2))
  | Ast.App(Ast.Ident name,args) ->
     let args = List.map (visit_exp env) args in
     (match List.assoc_opt name env with
      | Some (xs,e) -> List.fold_right2 (fun x v e -> Ast.Let(x,v,e)) xs args e
      | None -> Ast.App(Ast.Ident name,args))
  | Ast.App(e,args) -> Ast.App(visit_exp env e,List.map (visit_exp env) args) 
  | Ast.If(e1,e2,e3) -> Ast.If(visit_exp env e1,visit_exp env e2,visit_exp env e3) 
  | Ast.BinOp(op,e1,e2) -> Ast.BinOp(op,visit_exp env e1,visit_exp env e2) 
  | Ast.UnOp(op,e1) -> Ast.UnOp(op,visit_exp env e1) 
  | Ast.Block(xs) -> Ast.Block(List.map (visit_exp env) xs)
  | Ast.Seq(e1,e2) -> Ast.Seq(visit_exp env e1,visit_exp env e2)
  | Ast.While(e1,e2) -> Ast.While(visit_exp env e1,visit_exp env e2) 
  | Ast.Match(e,ms) -> 
     let ms = List.map
                (function
                    Ast.Case(c,e) ->
                     Ast.Case(c,visit_exp env e)
                  | Ast.Otherwise e ->
                     Ast.Otherwise (visit_exp env e)) ms in
     Ast.Match(visit_exp env e,ms) 
  | e -> e

(* idéalement, on aimerait pouvoir intégrer des fonctions d'autres modules,
              mais pour cela, il faudrait gérer les identificateurs de modules ...
   en voici une esquisse : 
   
let rec visit_modules mdls = 
  let _,rmdls =
    List.fold_left (fun (env,mdls) Ast.{mod_name;decls} -> 
        let env,decls = visit_decls env [] decls in 
        let env = List.map (fun (name,c) -> (mod_name ^ "." ^ name, c)) env in
        (env,(Ast.{mod_name;decls}::mdls))) ([],[]) mdls in
  List.rev rmdls 
*)
