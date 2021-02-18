(**************************************************************************)
(*                                                                        *)
(*         PSTL : OCaml sur la plate-forme Nand2Tetris (2020)             *)
(*                                                                        *)      
(*           Loïc SYLVESTRE              Pablito BELLO                    *)
(*           loic.sylvestre@etu.upmc.fr  pablito.bello@etu.upmc.fr        *)
(*                                                                        *)  
(**************************************************************************)

(* repris et étendu de :
   https://www.lri.fr/~filliatr/ens/compil/td/7/corrige/corrige.ml.html *)



open Types
open Past

let rec is_nonexpansive e = match e.exp_desc with
| Annotation (e,_) -> is_nonexpansive e
| Constant _ | Ident _ -> true
| Let (v,e1,e2) -> is_nonexpansive e1 && is_nonexpansive e2
| LetRec (v,e1,e2) -> is_nonexpansive e1 && is_nonexpansive e2
| Fun _ -> true
| App (e,es) -> is_nonexpansive e && List.for_all is_nonexpansive es
| If (e1,e2,e3) -> is_nonexpansive e1 && is_nonexpansive e2 && is_nonexpansive e3 
| Match (e1,_) -> is_nonexpansive e1  (* todo patern *)
| BinOp (_,e1,e2) -> is_nonexpansive e1 && is_nonexpansive e2
| UnOp (_,e1) -> is_nonexpansive e1
| Pair (e1,e2) -> is_nonexpansive e1 && is_nonexpansive e2
| _ -> false


let ty_of_repr venv ty = 
 let rec aux = function 
 | Tint -> Types.Tint
 | Tbool -> Types.Tbool
  | Tunit -> Types.Tunit
  | Tchar -> Types.Tchar
  | Tstring -> Types.Tstring
  | Tvar name -> (match List.assoc_opt name venv with
                  | None -> Types.Tvar (V.create ())
                  (* (Printf.printf "Error: Unbound type parameter %s.\n\n %sexit.\n" 
                                    name (Parseutils.string_of_position exp_loc);
                             exit 0); *)
                  | Some v -> v)
  | Tarrow (t1,t2) -> Types.Tarrow(aux t1,aux t2)
  | Tproduct (t1,t2) -> Types.Tproduct(aux t1,aux t2)
  | Tarray (t) -> Types.Tarray(aux t)
  | Tref (t) -> Types.Tref(aux t)
  | Tconstr (name,tylist) -> Types.Tconstr(name, List.map aux tylist)
  | Tident (name) -> Types.Tident(name)
in aux ty

let get_tyopt = function
| None -> Types.Tvar (V.create ()) 
| Some tyrepr -> ty_of_repr [] tyrepr

let unify_opt t tyopt loc = 
match tyopt with
| None -> ()
| Some ty -> unify t (ty_of_repr [] ty) loc

let initial_env primitives =
  List.fold_left
    (fun env (x,ty) -> add true x ty env) empty_env primitives;;

(* algorithme W *)
let rec w env decs = function
| [] -> decs
| d::ds -> 
     let xts = w_dec env d in
     let env = List.fold_left 
                  (fun env (x,t) -> 
                      let tc = canon t in
                       Printf.printf "%s : %s\n" x
                           (Past_print.sprint_real_ty 0 tc);
                      add false x tc env) env xts 
     in
     w env (decs @ xts) ds 
and w_dec env {decl_desc;decl_loc} = 
match decl_desc with
| DefVar ((x,tyopt),e) -> 
  let t = w_exp env e in
  unify_opt t tyopt decl_loc;
  ([(x,t)])
| DefFun funs -> 
  let funtys = List.map (fun f -> w_defun env f decl_loc) funs in
  (List.map2 (fun (f,_,_,_) tf -> (f,tf)) funs funtys)
| DefFunRec funs -> 
  let funcs = List.map (fun (x,_,_,e) -> x) funs in
  let env = List.fold_left (fun env f ->
                               let v0 = Types.Tvar (V.create ()) in 
                               add true f v0 env) env funcs in
  let env' = List.fold_left (fun env ((x,args,tyopt,e) as f) -> 
                               let t = w_defun env f decl_loc in 
                               add (is_nonexpansive e) x t env) env funs in
  List.map (fun (x,_,_,_) -> (x,find x decl_loc env')) funs
| Type (name,args,Exp_ty tyrepr) ->
  let vargs = List.map (fun var -> (var,Types.Tvar (V.create ()))) args in
  let ty = ty_of_repr vargs tyrepr in   (* Error: A type parameter occurs several times *)
  (Types.alias := (name,ty) :: !Types.alias); (* A REVOIR, c'EST OK *)
  []
  | Type (name,args,Sum cs) -> 
    let vargs = List.map (fun var -> 
                           (var,Types.Tvar (V.create ()))) args in 
    let tc = Types.Tconstr (name,List.map snd vargs) in (* Error: A type parameter occurs several times *)
    (Types.alias := (name,tc) :: !Types.alias);   (* ATTENTION ERREUR, type compatible avec tout *)
    List.map (fun (c,tys) -> 
                (c,List.fold_right (fun ty_repr tn -> let ty = ty_of_repr vargs ty_repr in
                                                      Types.Tarrow(ty,tn)) tys (* (Tident name) *)
                                                               tc)) cs
and w_defun env (f,args,tyropt,e) decl_loc = 
  let env' = List.fold_left 
               (fun env (xi,tyopt) -> 
                  let ty = get_tyopt tyopt in
                  add false xi ty env) env args in
  let tret = w_exp env' e in
  (match tyropt with
  | None -> ()
  | Some tyrepr -> let ty = ty_of_repr [] tyrepr in unify tret ty decl_loc);
  let t = List.fold_right (fun (xi,_) t -> 
                             let ti = find xi decl_loc env' in 
                             Types.Tarrow(ti,t))
           args tret in  t


and w_exp env {exp_desc;exp_loc} = 
let unify t1 t2 = unify t1 t2 exp_loc in
match exp_desc with
  | Annotation (e,tyrepr) -> 
    let ty = ty_of_repr [] tyrepr in
    let t1 = w_exp env e in
    unify ty t1;
    ty
  | Constant c -> (w_constant env exp_loc c)
  | Ident x -> 
    find x exp_loc env
  | Let ((x,tyopt_repr), e1, e2) ->
      let t1 = w_exp env e1 in
      let ty = get_tyopt tyopt_repr in
      unify t1 ty;
      w_exp (add (is_nonexpansive e1) x t1 env) e2
  | LetRec (name, e1, e2) -> 
  let tt = Types.Tvar (V.create ()) in
  let env1 = add (is_nonexpansive e1) name tt env in
  let t1 = w_exp env1 e1 in
  unify tt t1;
  w_exp env1 e2
  
  | Fun ((x,tyopt), e1) ->
      let ty = get_tyopt tyopt in
      let env = add false x ty env in
      let t1 = w_exp env e1 in
      Tarrow (ty, t1)
  | App (ef, es) ->
      let t1 = w_exp env ef in
      let ts = List.map (w_exp env) es in
      let v = Types.Tvar (V.create ()) in
      let t2 = List.fold_right (fun t acc -> Types.Tarrow (t,acc)) ts v in
      unify t1 t2;
      v
  | BinOp (op,e1,e2) ->
    let top = w_binop op in
    let t1 = w_exp env e1 in
    let t2 = w_exp env e2 in
    let v = Types.Tvar (V.create ()) in
    unify top (Types.Tarrow (t1,Types.Tarrow(t2,v)));
    v
  | UnOp (op,e1) ->
    let top = w_unop op in
    let t1 = w_exp env e1 in
    let v = Types.Tvar (V.create ()) in
    unify top (Types.Tarrow (t1,v));
    v
  | If (e1, e2, e3) -> 
      let t1 = w_exp env e1 in
      let t2 = w_exp env e2 in
      let t3 = w_exp env e3 in
      unify t1 Types.Tbool;
      unify t2 t3; t2
  | Match (e1,ms) ->
      let t1 = w_exp env e1 in
      let aux = function
      | Case(c,args,e) -> let tc = (w_constant env exp_loc c) in
                          let tret, tyargs, arity = 
                            let rec aux2 acc accn = function
                            | Types.Tarrow (ty,t) -> aux2 (ty::acc) (accn+1) t
                            | r -> (r,List.rev acc, accn) in aux2 [] 0 tc 
                          in

                          unify t1 tret;

                          let len = List.length args in
                          if arity != len then
                          (Printf.printf "Error : This constructor expects %d argument(s),\n\
                                          \ but is applied here to %d argument(s)\n\n%s. exit." arity len (Parseutils.string_of_position exp_loc);
                           exit 0);
                          let env = List.fold_right2 
                                      (fun x ty env -> add false x ty env) args tyargs env in
                       (* unify t (w_constant env exp_loc c); *)
                       w_exp env e
      | Otherwise e -> w_exp env e in
      (match ms with 
       | [] -> assert false
       | m::sm -> let tt = (aux m) in
                  List.iter (fun m -> unify (aux m) tt) sm; tt)
    (* .... *)
   | Pair(e1,e2) -> 
      let t1 = w_exp env e1 in
      let t2 = w_exp env e2 in
      Tproduct (t1,t2)   
   | Array_create (es) -> 
      let v = Types.Tvar (V.create ()) in 
      List.iter (fun e -> unify (w_exp env e) v) es; 
      Tarray v
   | Array_assign (e1,e2,e3) ->
     let t1 = w_exp env e1 in
     let t2 = w_exp env e2 in
     let t3 = w_exp env e3 in
     unify t2 Types.Tint;
     unify t1 (Types.Tarray t3);
     Tunit
   | Array_access (e1,e2) ->
     let t1 = w_exp env e1 in
     let t2 = w_exp env e2 in
     let v = Types.Tvar (V.create ()) in 
     unify t1 (Types.Tarray v);
     unify t2 Types.Tint;
     v
   | Ref(e1) -> 
     let t1 = w_exp env e1 in
     Tref t1
   | Ref_access(e1) ->
     let t1 = w_exp env e1 in
     let t = Types.Tvar (V.create ()) in 
     unify t1 (Types.Tref t);
     t
   | Ref_assign(e1,e2) ->
     let t1 = w_exp env e1 in
     let t2 = w_exp env e2 in
     unify t1 (Types.Tref t2);
     Types.Tunit
   (* | String ->  *)
   | Seq(e1,e2) -> 
     let t1 = w_exp env e1 in
     let t2 = w_exp env e2 in
     unify t1 Types.Tunit;
     t2
   | While(e1,e2) ->
     let t1 = w_exp env e1 in
     let t2 = w_exp env e2 in
     unify t1 Types.Tbool;
     unify t2 Types.Tunit;
     Tunit
   | For(x,e1,e2,e3) ->
     let t1 = w_exp env e1 in
     let t2 = w_exp env e2 in
     unify t1 Types.Tint;
     unify t2
     Types.Tint;
     let env' = add false x Types.Tint env in (* ou is_nonexpansive e1 ? *) 
     let t3 = w_exp env' e3 in
     unify t3 Types.Tunit;
     Types.Tunit
   | Assert (e,_) -> 
     let ty = w_exp env e in 
     unify ty Types.Tbool; 
     Types.Tvar (V.create ())
   | Magic e -> let _ = w_exp env e in Types.Tvar (V.create ())

and w_constant env exp_loc = function
| Unit -> Types.Tunit
| Bool _ -> Types.Tbool
| Int _ -> Types.Tint 
| Char _ -> Types.Tchar
| String _ -> Types.Tstring
| Array_empty -> let v = Types.Tvar (V.create ()) in Types.Tarray v
| Constr s -> find s exp_loc env

and w_binop = function
| Add -> Tarrow(Tint,Tarrow(Tint,Tint))
| Minus -> Tarrow(Tint,Tarrow(Tint,Tint))
| Mult -> Tarrow(Tint,Tarrow(Tint,Tint))
| Div -> Tarrow(Tint,Tarrow(Tint,Tint))
| Lt -> Tarrow(Tint,Tarrow(Tint,Tbool))
| Le -> Tarrow(Tint,Tarrow(Tint,Tbool))
| Neq -> Tarrow(Tint,Tarrow(Tint,Tbool))
| Eq -> Tarrow(Tint,Tarrow(Tint,Tbool))
| Ge -> Tarrow(Tint,Tarrow(Tint,Tbool))
| Gt -> Tarrow(Tint,Tarrow(Tint,Tbool))
| Or -> Tarrow(Tbool,Tarrow(Tbool,Tbool))
| And -> Tarrow(Tbool,Tarrow(Tbool,Tbool))
| Lor -> Tarrow(Tint,Tarrow(Tint,Tint))
| Land -> Tarrow(Tint,Tarrow(Tint,Tint))

and w_unop = function
| UMinus -> Tarrow(Tint,Tint)

(* initial_env prims *) 

let type_check {decls;mod_name} env = 
  Printf.printf "=== module %s ==================.\n" mod_name;
  try let decs = w env [] decls in 
      let env = List.fold_left (fun env (x,t) -> 
        add true (mod_name ^ "." ^ x) (canon t) env) env decs
      in 
      Types.old_alias := (List.map (fun (id,ty) -> let id = mod_name ^ "." ^ id in (id,ty)) !Types.alias)  @ !Types.old_alias;
      env
  with 
  | UnificationFailure (t1,t2,loc) -> 
      Printf.printf "\nError: %s\nThis expression has type %s but an expression was expected of type
         %s\n" (Parseutils.string_of_position loc) (Past_print.sprint_real_ty 0 t1) (Past_print.sprint_real_ty 0 t2); exit 0
  | Unbound_value (x,loc) -> Printf.printf "Error: %s\nUnbound value %s\n" (Parseutils.string_of_position loc)  x; exit 0
  | Types.Alias_not_found(name,loc) -> Printf.printf "Error: %s\n alias not found %s\n" (Parseutils.string_of_position loc)  name; exit 0
  | exn -> Printf.printf "UN BUG DANS LE TYPEUR. on continue : %s\n" ( Printexc.to_string exn); env
