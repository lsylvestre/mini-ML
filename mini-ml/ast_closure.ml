(**************************************************************************)
(*                                                                        *)
(*         PSTL : OCaml sur la plate-forme Nand2Tetris (2020)             *)
(*                                                                        *)      
(*           Loïc SYLVESTRE              Pablito BELLO                    *)
(*           loic.sylvestre@etu.upmc.fr  pablito.bello@etu.upmc.fr        *)
(*                                                                        *)  
(**************************************************************************)

let gen = Gensym.create 10000

let rec rw_exp env e = 
  match e with
  | Ast.Ident (name) -> Ast.Ident name
  | Ast.Fun (name,e) -> 
     (match env with 
      (* [] -> optimisation si pas de variables libres ? *)
      | _ -> let adr = Gensym.next_int gen in
             let code = rw_exp (name :: env) e in
             let closure_env = List.map (fun name -> Ast.Ident(name)) env in
             let closure = Ast.Constant(Ast.Int adr) :: closure_env in
             Ast.Closure((adr,code),name,Ast.Block(closure)))
  | Ast.LetRec (f,e1,e2) -> Ast.LetRec (f,rw_exp (f :: env) e1,rw_exp (f :: env) e2)
(* Il n'y a pas, pour le moment, de fermeture récursive.
     Voilà une piste de travail :
                          
    let adr = Gensym.next_int gen in
    let code = rw_exp (name :: env) e1 in
    let closure_env = Ast.Ident(f) :: List.map
                                        (fun name -> Ast.Ident(name))
                                        (List.filter ((<>) f) env) in
    let closure = Ast.Constant(Ast.Int adr) :: closure_env in
    let clos = Ast.Closure((adr,code),name,Ast.Block(closure)) in
    let x = Gensym.next "clos" gen in
    Ast.LetRec(f,"fake", 
               Ast.Let(x,clos,
                       Ast.Seq(Ast.App(Ast.Ident("Internal.array_set"),
                                       [Ast.Ident(x);
                                        Ast.Constant(Ast.Int(1));
                                        Ast.Ident(x)]),
                               Ast.Ident(x))),
               e2) 
 *)
  | Ast.Constant(c) -> Ast.Constant(c)
  | Ast.Let(v,e1,e2) -> Ast.Let(v,rw_exp env e1,rw_exp env e2)
  | Ast.App(e,args) -> Ast.App(rw_exp env e,List.map (rw_exp env) args) 
  | Ast.If(e1,e2,e3) -> Ast.If(rw_exp env e1,rw_exp env e2,rw_exp env e3)
  | Ast.BinOp(op,e1,e2) -> Ast.BinOp(op,rw_exp env e1,rw_exp env e2)
  | Ast.UnOp(op,e1) -> Ast.UnOp(op,rw_exp env e1)
  | Ast.Block(xs) -> Ast.Block(List.map (rw_exp env) xs)
  | Ast.Seq(e1,e2) -> Ast.Seq(rw_exp env e1,rw_exp env e2)
  | Ast.While(e1,e2) -> Ast.While(rw_exp env e1,rw_exp env e2)
  | Ast.Match(e,ms) -> 
     Ast.Match (rw_exp env e, 
                List.map (fun m ->
                    match m with 
                    | Ast.Case(c,e) -> Ast.Case(c,rw_exp env e)
                    | Ast.Otherwise(e) -> Ast.Otherwise(rw_exp env e)) ms) 
  | _ -> e (* supposer ne pas contenir de valeur fonctionnelle à transformer. 
              (à vérifier) *)

let rw_fundecs l = 
  List.map (fun d -> 
      match d with 
      | Ast.DF (name,args,e) -> Ast.DF (name,args,(rw_exp args) e)) l 

let rw_decl d = 
  match d with
  | Ast.DefVar(v,e) -> Ast.DefVar(v,rw_exp [] e)
  | Ast.DefFun(l) -> Ast.DefFun (rw_fundecs l)
  | Ast.DefFunRec(l) -> Ast.DefFunRec (rw_fundecs l)
  | _ -> d

let rewrite m =
  match m with Ast.Module(mod_name,decls) ->
    let decls = List.map rw_decl decls in
    Ast.Module(mod_name,decls)
