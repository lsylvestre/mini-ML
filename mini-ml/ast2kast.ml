(**************************************************************************)
(*                                                                        *)
(*         PSTL : OCaml sur la plate-forme Nand2Tetris (2020)             *)
(*                                                                        *)      
(*           Loïc SYLVESTRE              Pablito BELLO                    *)
(*           loic.sylvestre@etu.upmc.fr  pablito.bello@etu.upmc.fr        *)
(*                                                                        *)  
(**************************************************************************)


let gen = Gensym.create 0


let rec associate_aux acc n l = 
  match l with
  | [] -> acc    (* inverse l'ordre, pas génant *)
  | h::t -> let r = (h,n) :: acc in 
            associate_aux r (n+1) t

let associate n l =
  associate_aux [] n l

type genv = Genv of (Ast.name * 
                    (Ast.name * int) list * 
                    string list * 
                    (Ast.name * Ast.name) list * 
                     Ast.name list)

let empty_genv prims mod_name =
  Genv (mod_name,[],[],prims,[])
  
let genv_extend genv x = 
  match genv with
  | Genv (md,globals,g,p,init) -> 
    (match globals with 
    | [] -> let globals = ((x,0)::[]) in
            (0,Genv (md,globals,g,p,init))
    | pi::_ -> let i = snd pi + 1 in
                  let globals = ((x,i)::globals) in
                  (i,Genv (md,globals,g,p,init)))

(*   lenv  *)
(*    arguments : (Ast.name * int) list              *)
(*    locals : (Ast.name * int) list                 *)
(*    free : (Ast.name * int) list                   *)

type lenv = Lenv of ((Ast.name * int) list * 
                     (Ast.name * int) list * 
                     (Ast.name * int) list)

let empty_lenv =
  Lenv([],[],[])
  
let frame args = 
  let a = List.mapi (fun i -> fun name -> (name,i)) args in
  Lenv (a,[],[])

let lenv_extend x lenv = 
  match lenv with 
  Lenv(args,locals,free) -> 
  (match locals with
  | [] -> (0,Lenv(args,((x,0) :: []),free))
  | pi::_ -> let i = snd pi + 1 in 
             (i,Lenv(args,((x,i) :: locals),free)))

let lenv_extend_tail x lenv = lenv_extend x lenv
(* ATTENTION : cette optimisation provoque des captures de variables !! 
  match lenv with 
  Lenv(args,locals,free) -> 
  (match locals with
  | [] -> (0,Lenv(args,((x,0) :: []),free))
  | pi::_ -> let i = snd pi in 
             let i = (match List.assoc_opt x locals with 
                      | None -> i + 1
                      | Some(i) -> i) in
             (i,Lenv(args,((x,i) :: locals),free))) *)


let compare_ast_int c1 c2 =
  match c1 with
  | Ast.Int(n1) -> (match c2 with
                    | Ast.Int(n2) -> Pervasives.compare n1 n2
                    | _ -> assert false)
  | _ -> assert false

let rec rw_constant lenv genv cst = 
match cst with 
 | Ast.Unit -> Kast.Constant(Kast.Unit)
 | Ast.Int(n) -> Kast.Constant(Kast.Int n)
 | Ast.Char(c) -> Kast.Constant(Kast.Int (Pervasives.int_of_char c))
 | Ast.Bool(b) -> Kast.Constant(Kast.Bool b)
 | Ast.Array_empty -> Kast.Constant(Kast.Array_empty)
 | Ast.String(s) ->
   let rev_xs = ref [] in
   String.iter (fun c -> rev_xs := (c :: (!rev_xs))) s;
   let create_string = 
     Ast.Block(List.rev_map (fun c -> Ast.Constant(Ast.Char(c))) (!rev_xs)) 
   in
   rw_exp lenv genv create_string 

and rw_exp lenv genv e = 
  match e with
  | Ast.Constant(c) -> rw_constant lenv genv c
  | Ast.Ident(name) ->
     (match genv with
      | Genv (mod_name,globals,global_funs,primitives,_) -> 
         (match lenv with 
            Lenv(args,locals,free) -> 
            (match List.assoc_opt name locals with
             | None ->
                (match List.assoc_opt name args with
                 | None ->
                    (match List.assoc_opt name free with
                     | None ->
                        (match List.assoc_opt name globals with
                         | None -> let full_name =
                                     if (match String.index_opt name '.' with
                                         | None -> false
                                         | _ -> true)
                                     then name
                                     else ((^) ((^) mod_name  ".") name)
                                   in                
                                   (if List.mem full_name global_funs
                                    then Kast.GFun(full_name)
                                    else let f =
                                           match List.assoc_opt name primitives with 
                                           | None -> failwith ((^) "cannot find " name)
                                           | Some(f) -> f
                                         in
                                         Kast.GFun(f))
                         | Some(i) -> Kast.Variable(Kast.Global ((^) ((^) mod_name ".") name))) (* Kast.App (Kast.GFun(genv.mod_name ^ "." ^ name),[])) *)
                     | Some(i) -> 
                        Kast.Variable(Kast.Free (i))) 
                 | Some(i) -> Kast.Variable(Kast.Argument (i)))
             | Some(i) -> Kast.Variable(Kast.Local i)))) 
  | Ast.Let(name,e1,e2) ->
     let p = lenv_extend_tail name lenv in (* optimisation (avec `lenv_extend_tail`) : recyclage des variables masquées, 
                                                   contrainte pour la génération de code : dans, C[let x = e1(x) in e2] C[e1] doit bien manipuler le nouveau [x] et [e1] l'ancien *)
     let i = fst p in
     let lenv' = snd p in
     Kast.Let(i,rw_exp lenv genv e1, rw_exp lenv' genv e2)
  | Ast.LetRec(f,e1,e2) -> 
     let (i,lenv') = lenv_extend f lenv in
     let ke1 = rw_exp lenv' genv e1 in
     let ke2 = rw_exp lenv' genv e2 in
     Kast.Let (i,ke1,
               Kast.Seq(Kast.App(Kast.GFun("Internal.array_set"),[Kast.Variable(Kast.Local(i));Kast.Constant(Kast.Int(i+1));Kast.Variable(Kast.Local(i))]),
                        ke2))
  (* let (i,lenv') = lenv_extend f lenv in
  let (j,lenv2) = lenv_extend "__fake__" lenv' in
  let ke1 = rw_exp lenv2 genv e1 in
  let ke2 = rw_exp lenv2 genv e2 in
  Kast.Let (j,ke1,Kast.Seq(Kast.App(Kast.GFun("Internal.array_set"),[Kast.Variable(Kast.Local(j));Kast.Constant(Kast.Int(i-1));Kast.Variable(Kast.Local(j))]),
                          ke2)) *)


  (*
  let p = lenv_extend f lenv in (* tail ? .optimisation (avec `lenv_extend_tail`) : recyclage des variables masquées, 
                                                   contrainte pour la génération de code : dans, C[let x = e1(x) in e2] C[e1] doit bien manipuler le nouveau [x] et [e1] l'ancien *)
     let i = fst p in
     let lenv' = snd p in

  let p2 = lenv_extend name lenv' in
  let j = fst p2 in
  let lenv2 = snd p2 in
     Kast.Let(i,rw_exp lenv2 genv e1, rw_exp lenv' genv e2) *)
  | Ast.Fun(_,_) -> assert false (* déjà transformer en fermeture *)

  | Ast.Closure(code,name,v) ->
     let id = fst code in
     let c = snd code in
     (* après lambda-lifting *)
     let lenv_code = 
       match lenv with
       | Lenv(args,locals,free) ->
          let free = associate 1
                       (match v with 
                        | Ast.Block(l) ->
                           (match l with
                            | [] -> assert false
                            | addr::l -> List.map (fun e ->
                                                  match e with
                                                  | Ast.Ident(sym) -> sym
                                                  | _ -> assert false) l)
                      | _ -> assert false) in
         let args=(name,1)::[] in
         let locals=[] in
         Lenv(args,locals,free)
     in
     let lenv_v = snd (lenv_extend name lenv) in
     let kc = rw_exp lenv_code genv c in
     let kv = rw_exp lenv_v genv v in (* lenv_v *)
     Kast.Closure((id,kc),kv)
  | Ast.App(e,args) ->
     Kast.App(rw_exp lenv genv e, List.map (fun e -> rw_exp lenv genv e) args)
  | Ast.If(e1,e2,e3) ->
     Kast.If(rw_exp lenv genv e1,
             rw_exp lenv genv e2,
             rw_exp lenv genv e3)
  | Ast.BinOp(op,e1,e2) ->
     (match op with  
      (* les primitives `or` et `and` de la VM necessite le calcule des deux arguments *)
      (* on réécrit donc le `or` et `and` de mini-ml avec des if then else *)
      | Ast.Or -> 
        rw_exp lenv genv (Ast.If (e1,Ast.Constant(Ast.Bool(true)),e2))
      | Ast.And -> rw_exp lenv genv (Ast.If (e1,e2,Ast.Constant(Ast.Bool(false))))
      | _ -> Kast.BinOp(op,rw_exp lenv genv e1,rw_exp lenv genv e2))
  | Ast.UnOp(op,e1) -> Kast.UnOp(op,rw_exp lenv genv e1)
  | Ast.Ext(ext) ->
     (match ext with 
      | Ast.Array_alloc(e) ->
         let array_alloc = Ast.App(Ast.Ident("Internal.array_create_uninitialized"),e::[]) in
         rw_exp lenv genv array_alloc
      | Ast.SetGlobal(e,i) ->
         Kast.Ext(Kast.SetGlobal (rw_exp lenv genv e,i))
      | Ast.ReadGlobal(i) ->
         Kast.Ext(Kast.ReadGlobal(i))
      | Ast.Label(s,e) -> 
         Kast.Ext(Kast.Label(s,rw_exp lenv genv e))
      | Ast.Goto(s,xs) -> 
         Kast.Ext(Kast.Goto(s,List.map (fun e -> rw_exp lenv genv e) xs)))
  | Ast.Seq(e1,e2) ->
     Kast.Seq(rw_exp lenv genv e1, rw_exp lenv genv e2)
  | Ast.While(e1,e2) ->
     Kast.While(rw_exp lenv genv e1, rw_exp lenv genv e2)

 | Ast.Block(xs) ->
     rw_exp lenv genv (
       let n = List.length xs in
       let a = Gensym.next "tmp" gen in
       Ast.Let(a,Ast.Ext(Ast.Array_alloc(Ast.Constant(Ast.Int(n)))),
               aux_ast_block a 0 xs))  

  | Ast.Match(e,ms) -> 
     let p = aux_ast_match [] ms in
     let ms = fst p in
     let otherw = snd p in 
     let sms = List.map (fun m -> 
                           match m with 
                           | Ast.Case(c,e) -> (c,e)
                               (*  (match c with 
                                 | Ast.Int(n) -> (Ast.Int(n),e) 
                                 | _ -> (c,e)) *)
                              (* NB : on va trier les constantes : mais avant celà, *)
                              (* il faut absolument renommer les constructeurs par leur entier associé, *)
                              (* sinon, la liste ne sera pas triée correctement et cela produira des bugs *)
                              (* dans la génération de code *)
                           | _ -> assert false) ms
     in
     let smst = List.sort (fun p1 -> fun p2 -> compare_ast_int (fst p1) (fst p2)) sms in
     let var = Gensym.next "L" gen in
     rw_exp lenv genv (Ast.Let(var,e,aux_ast_match2 otherw var smst))


and aux_ast_block a i l =  (* pas de fonction récursives locales ... *)
match l with
 | [] -> Ast.Ident(a)
 | e::es -> Ast.Seq(Ast.App (Ast.Ident("Internal.array_set"),
                            (Ast.Ident(a) ::
                             Ast.Constant(Ast.Int(i)) ::
                             e :: [])),
            aux_ast_block a (i+1) es)


and aux_ast_match acc l = 
 match l with
  | [] -> (acc,None)
  | h::t -> (match h with
            | Ast.Otherwise(e) -> (acc,Some e)
            | _ -> aux_ast_match (h::acc) t) 


and aux_ast_match2 otherw var ms = 
   match ms with 
   | [] -> (match otherw with 
            | None ->
               Ast.App(Ast.Ident("Pervasives.exit"),
                       (Ast.Constant(Ast.Int(1))::[])) (* match failure *)
            | Some(e) -> e)
   | p::l -> (match l with 
              | [] -> snd p
              | _ -> let p = List.nth ms (List.length ms / 2) in
          let md = fst p in
          let e1 = snd p in
           let pls = List.partition (fun p -> fst p < md) ms in
           let l1 = fst pls in
           let l2 = snd pls in 
           let l2t = List.tl l2 in (* j'enleve celui qui est égal *)
           Ast.If(Ast.BinOp(Ast.Lt,
                            Ast.Ident(var),
                            Ast.Constant(md)),
                  aux_ast_match2 otherw var l1,
                  Ast.If(Ast.BinOp(Ast.Eq,
                                   Ast.Ident(var),
                                   Ast.Constant(md)),e1,aux_ast_match2 otherw var l2t)))
           
type recflag = Rec | NonRec


let rw_defun mod_name genv recflag dfs =
  let gnames = List.map
                 (fun d -> match d with
                           | Ast.DF (name,args,e) -> (^) ((^) mod_name ".") name)  dfs in
  let genv2 = match genv with
              | Genv (md,globals,global_funs,p,init) -> 
                let global_funs = List.append gnames global_funs in
                Genv (md,globals,global_funs,p,init) in
  let by = List.concat
             (List.map
                (fun d -> 
                  match d with 
                  | Ast.DF (name,args,e) ->
                  let lenv = frame args in
                  let ke = rw_exp lenv
                             (match recflag with 
                              | Rec -> genv2 
                              | NonRec -> genv) e
                  in (* genv si non recursif *)
                  let arity = List.length args in
                  (Kast.DefFun (name,arity,ke))::[]) dfs) in
  (genv2,by)

let rec rw_decl mod_name genv d = 
  match d with
  | Ast.DefVar (name,e) -> 
    if String.equal name "_" 
    then rw_decl mod_name genv (
         let name = Gensym.next "voidExpr" gen in
         Ast.DefVar (name,e))
    else 
     let name_init = (^) "__init__" name in
     let p = genv_extend genv name in
     let i = fst p in
     let genv = snd p in
     let genv = 
       match genv with
       | Genv (md,globals,g,p,init) -> 
          let init = ( (^) ((^) mod_name ".") name_init) :: init in
         Genv (md,globals,g,p,init)
     in
     let p = rw_decl
               mod_name
               genv
               (Ast.DefFun (Ast.DF (name_init,[],Ast.Ext(Ast.SetGlobal(e,i))) :: [])) in
     let genv = fst p in
     let d1 = snd p in
     let p = rw_decl
               mod_name
               genv
               (Ast.DefFun (Ast.DF (name,[],Ast.Ext(Ast.ReadGlobal(i))) :: [])) in
     let genv = fst p in
     let d2 = snd p in
     (genv,List.append d1 d2)
  | Ast.DefFun (l) -> rw_defun mod_name genv NonRec l 
  | Ast.DefFunRec (l) -> rw_defun mod_name genv Rec l
  | _ -> (genv,[])



and rw_decls mod_name genv ds = 
  let p =
    List.fold_left 
      (fun p -> fun d -> 
          let genv = fst p in
          let acc = snd p in
          let p0 = rw_decl mod_name genv d in
          let genv = fst p0 in
          let kds = snd p0 in
          (match snd p0 with
          | [] -> (genv,acc)
          | _ -> (genv,List.append kds acc))) (genv,[]) ds in
  let genv = fst p in
  let kds = snd p in
  (genv,List.rev kds)



(* convertit l IAST du module m en KAST *)
let rec rewrite genv m =
  match m with Ast.Module(mod_name,decls) ->
    let p = rw_decls mod_name genv decls in
    let genv = fst p in
    let kds = snd p in
    match genv with
    | Genv (_,globals,_,_,_) -> 
     (genv,Kast.Module(mod_name,kds,(List.map fst globals)))
