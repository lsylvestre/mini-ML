(**************************************************************************)
(*                                                                        *)
(*         PSTL : OCaml sur la plate-forme Nand2Tetris (2020)             *)
(*                                                                        *)      
(*           Loïc SYLVESTRE              Pablito BELLO                    *)
(*           loic.sylvestre@etu.upmc.fr  pablito.bello@etu.upmc.fr        *)
(*                                                                        *)  
(**************************************************************************)

let gen = Gensym.create 0

let create () = ref []

let rec rw_exp cl env lenv e = 

  match e with
  | Ast.Fun (name,e) ->
    let env = List.append lenv env in
    let lenv = name :: [] in
    let e = rw_exp cl env lenv e in
    let sym = Gensym.next "_lambda" gen in
    let vars = Freevr.collect env lenv e in
    (match vars with 
     (* | [] -> Ast.Fun (name,e)     // une optimisation. ok? *)
     | _ ->  let f = List.fold_right
                       (fun x -> fun e -> Ast.Fun (x,e)) (List.append vars [name]) e
             in
             let d = Ast.DefVar(sym,f) in
             (* ici d est une variable globale transportant une abstraction.
                on pourrait vouloir faire de d une fonction globale, comme ceci :

                let d = Ast.DefFun [Ast.DF (sym,[name], f)]
             *)
             cl := d :: !cl; 
             Ast.App(Ast.Ident (sym),List.map (fun v -> Ast.Ident(v)) vars))     
  | Ast.Let(name,e1,e2) -> let lenv' = name :: lenv in
                           Ast.Let(name,rw_exp cl env lenv e1,rw_exp cl env lenv' e2)
  | Ast.LetRec(name,e1,e2) -> let lenv' = name :: lenv in
                              Ast.LetRec (name,
                                          rw_exp cl env lenv' e1,
                                          rw_exp cl env lenv' e2)
  | Ast.Ident(name) -> Ast.Ident(name)
  | Ast.Constant(c) -> Ast.Constant(c)
  | Ast.App(e,args) -> Ast.App(rw_exp cl env lenv e,
                               List.map (rw_exp cl env lenv) args) 
  | Ast.If(e1,e2,e3) -> Ast.If(rw_exp cl env lenv e1,
                               rw_exp cl env lenv e2,
                               rw_exp cl env lenv e3)
  | Ast.BinOp(op,e1,e2) -> Ast.BinOp(op,rw_exp cl env lenv e1,
                                     rw_exp cl env lenv e2)
  | Ast.UnOp(op,e1) -> Ast.UnOp(op,rw_exp cl env lenv e1)
  | Ast.Block(xs) -> Ast.Block(List.map (rw_exp cl env lenv) xs)
  | Ast.Seq(e1,e2) ->
     if (match e1 with 
         | Ast.Constant(c) -> (match c with
                                 Ast.Unit -> true
                               | _ -> false) 
         | _ -> false)
     then rw_exp cl env lenv e2
     else Ast.Seq(rw_exp cl env lenv e1,rw_exp cl env lenv e2)
  | Ast.While(e1,e2) -> Ast.While(rw_exp cl env lenv e1,rw_exp cl env lenv e2)
  | Ast.Match(e,ms) ->
     Ast.Match (rw_exp cl env lenv e,
                List.map
                  (fun m ->
                    match m with
                    | Ast.Case(c,e) ->
                       Ast.Case(c,rw_exp cl env lenv e) 
                    | Ast.Otherwise(e) ->
                                      Ast.Otherwise(rw_exp cl env lenv e)) ms) 
  | _ -> e

       
let rec rw_decl cl d =
  match d with
  | Ast.DefVar(v,e) -> Ast.DefVar(v,rw_exp cl [] [] e)
  | Ast.DefFun(l) -> Ast.DefFun (rw_fundecs cl l)
  | Ast.DefFunRec(l) -> Ast.DefFunRec (rw_fundecs cl l)
  | _ -> d
and rw_fundecs cl lf = 
  List.map (fun d -> match d with
                     | Ast.DF (name,args,e) ->
                        Ast.DF (name,args,rw_exp cl [] [] e)) lf
  
let rec rewrite m =
  match m with 
  | Ast.Module(mod_name,decls) ->
    
    let decls = List.concat (
                    List.map (fun d ->
                        let collect = create () in 
                        let d = rw_decl collect d in
                        List.rev_append (* le reverse est important *)
                          (!collect)
                          (d::[]))
                      decls) in 
    Ast.Module(mod_name,decls)
