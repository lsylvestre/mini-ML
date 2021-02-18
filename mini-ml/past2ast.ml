(**************************************************************************)
(*                                                                        *)
(*         PSTL : OCaml sur la plate-forme Nand2Tetris (2020)             *)
(*                                                                        *)      
(*           Loïc SYLVESTRE              Pablito BELLO                    *)
(*           loic.sylvestre@etu.upmc.fr  pablito.bello@etu.upmc.fr        *)
(*                                                                        *)  
(**************************************************************************)

let gen = Gensym.create 0

let compile_assertions = ref false

let assoc_constructor name (genv,lenv) =
  match List.assoc_opt name lenv with 
  | None -> (match List.assoc_opt name genv with 
             | None -> (print_string ("Error: Unbound constructor "
                                      ^ name
                                      ^ ". exit.\n");
                        exit 0)
             | Some (id,arity) -> (id,arity))
  | Some (id,arity) -> (id,arity)
                     
let export mod_name (genv,lenv) =
  let genv = (List.map (fun (c,j) ->
                  let c = mod_name ^ "." ^ c in (c,j)) lenv) @ genv in
  (genv,[])

let env_extend v (genv,lenv) = (genv,v::lenv)


let ast_ref_contents e =
  Ast.App(Ast.Ident("Internal.array_get"),[e;Ast.Constant(Ast.Int 0)])

let ast_ref e =
  Ast.App(Ast.Ident("Internal.array_make"),[Ast.Constant (Ast.Int 1);e])

let rec visit_tmodule env Past.{mod_name;decls} = 
  let rec aux acc env = function (* map filter *)
    | [] -> (env,List.rev acc)  
    | Past.{decl_desc}::ds -> 
       (match decl_desc with 
        | Past.DefVar((name,_),e) -> 
           aux (Ast.DefVar(name,rw_exp env e) :: acc) env ds
        | Past.DefFun(l) -> 
           aux (Ast.DefFun (visit_fundecs env l) :: acc) env ds
        | Past.DefFunRec(l) -> 
           aux (Ast.DefFunRec ((visit_fundecs env l)) :: acc) env ds
        | Past.Type (s,lvs,ty) -> 
           aux acc (match ty with 
                    | Past.Exp_ty (_) -> env
                    | Past.Sum (ls) ->
                       (let rec aux2 n env lcs = 
                          match lcs with 
                          | [] -> env
                          | (name,tys)::cs -> 
                             let size = List.length tys in
                             aux2 (n+1) (env_extend (name,(n,size)) env) cs 
                        in
                        aux2 0 env ls)) ds)
  in 
  let env,ds = aux [] env decls in
  let env = export mod_name env in
  (env,Ast.Module(mod_name,ds))

and visit_fundecs env l = 
  List.map (fun (name,args,_,e) -> Ast.DF (name,List.map fst args,rw_exp env e)) l 
and rw_exp env Past.{exp_desc;exp_loc} =
  match exp_desc with
  | Past.Ident name -> Ast.Ident name
  | Past.Annotation (e,_) -> rw_exp env e
  | Past.Constant c -> Ast.Constant(rw_cst env c)
  | Past.Let((name,_),e1,e2) ->  Ast.Let(name,rw_exp env e1,rw_exp env e2)
  | Past.LetRec(name,e1,e2) ->  Ast.LetRec(name,rw_exp env e1,rw_exp env e2)
  | Past.App(e,args) -> 
     let args = List.map (rw_exp env) args in 
     (match e with 
      | Past.{exp_desc=Past.Constant(Past.Constr name)} -> 
         (let id,arity = assoc_constructor name env in
          let n = List.length args in
          let k = arity - n in 
          let rec aux extra = function
            | 0 -> Ast.Block(Ast.Constant(Ast.Int(id)) :: args @ extra)
            | k -> assert (n > 0);
                   let name = Gensym.next "__cstparam" gen in
                   Ast.Fun (name,aux (Ast.Ident(name)::extra) (k-1)) 
          in aux [] k)
      | _ -> Ast.App(rw_exp env e,args))
  | Past.Fun ((name,_),e) -> Ast.Fun (name,rw_exp env e) 
  | Past.If(e1,e2,e3) -> Ast.If(rw_exp env e1,rw_exp env e2,rw_exp env e3)
  | Past.BinOp(op,e1,e2) -> Ast.BinOp(visit_binop op,rw_exp env e1,rw_exp env e2)
  | Past.UnOp(op,e1) -> Ast.UnOp(visit_unop op,rw_exp env e1)
  | Past.Ref_access(e1) -> ast_ref_contents (rw_exp env e1)
  | Past.Ref_assign(e1,e2) ->
     Ast.App(Ast.Ident("Internal.array_set"),
             [rw_exp env e1;Ast.Constant(Ast.Int(0));rw_exp env e2])
  | Past.Ref(e) -> ast_ref (rw_exp env e)
  | Past.Array_access(e1,e2) ->
     Ast.App(Ast.Ident("Internal.array_get"),[rw_exp env e1;rw_exp env e2])
  | Past.Array_assign(e1,e2,e3) ->
     Ast.App(Ast.Ident("Internal.array_set"),
             [rw_exp env e1;rw_exp env e2;rw_exp env e3])
  | Past.Pair(e1,e2) -> Ast.Block([rw_exp env e1;rw_exp env e2]) 
  | Past.Array_create(xs) -> Ast.Block(List.map (rw_exp env) xs)
  | Past.Seq(e1,e2) -> Ast.Seq(rw_exp env e1,rw_exp env e2)
  | Past.While(e1,e2) -> Ast.While(rw_exp env e1,rw_exp env e2)
  | Past.For(name,e0,e1,e2) ->
     let name_zz = Gensym.next name gen in
     let len_zz = Gensym.next "L" gen in
     Ast.Let(
         name_zz, 
         ast_ref (rw_exp env e0),
         Ast.Let(len_zz,rw_exp env e1,
                 Ast.While(Ast.BinOp
                             (Ast.Le,
                              ast_ref_contents (Ast.Ident(name_zz)),
                              Ast.Ident(len_zz)),
                           Ast.Let(name, ast_ref_contents (Ast.Ident(name_zz)),
                                   Ast.Seq(
                                       rw_exp env e2,
                                       Ast.App(Ast.Ident("Internal.array_set"), 
                                               [Ast.Ident(name_zz); 
                                                Ast.Constant(Ast.Int 0);
                                                Ast.BinOp(
                                                    Ast.Add,
                                                    Ast.App(
                                                        Ast.Ident("Internal.array_get"), 
                                                        [Ast.Ident(name_zz);
                                                         Ast.Constant(Ast.Int 0)]),
                                                    Ast.Constant(Ast.Int 1))])))))) 
     
  | Past.Match (e,ms) -> visit_match env e ms
  | Past.Assert(e,pos) -> (* prevoir l'accès au nom du module *)
     if not !compile_assertions 
     then  Ast.Constant(Ast.Unit)
     else
       Ast.If(rw_exp env e,
              Ast.Constant(Ast.Unit),
              Ast.Seq(
                  Ast.App(Ast.Ident("Internal.print_char_array"),
                          [Ast.Constant
                             (Ast.String 
                                (Printf.sprintf "assertion fail [%s]"
                                   (Past_print.sprint_exp 0 e)))]),
                  Ast.Seq(Ast.App(Ast.Ident("Internal.print_newline"),
                                  [Ast.Constant(Ast.Unit)]),
                          Ast.Seq(
                              Ast.App(Ast.Ident("Internal.print_char_array"),
                                      [Ast.Constant
                                         (Ast.String 
                                            (Printf.sprintf "at %s. exit."
                                               (Parseutils.string_of_position pos)))]),
                              Ast.App(Ast.Ident("Internal.exit"),
                                      [Ast.Constant (Ast.Int(0))])))))
  | Past.Magic(e) -> rw_exp env e
and visit_match env ec ms =
  (* possiblité d'éviter un let dans le cas (match x with ...) où x est un ident *)
  let name = Gensym.next "_match" gen in
  Ast.Let (
      name,
      rw_exp env ec,
      Ast.Match (
          Ast.If(
              Ast.BinOp(
                  Ast.Le,
                  Ast.Ident(name),
                  Ast.Constant(Ast.Int(256))),
              Ast.Ident(name),
              Ast.App(Ast.Ident("Internal.array_get"),[
                    Ast.Ident(name);
                    Ast.Constant(Ast.Int(0))])),
          List.map 
            (function 
             | Past.Case(c,[],e) -> Ast.Case(rw_cst env c,rw_exp env e)
             | Past.Case(c,args,e) -> 
                let e = rw_exp env e in
                let e' = List.fold_right2 (fun arg v e -> Ast.Let (arg,v,e)) 
                           args (List.mapi (fun i _ ->
                                     Ast.App(Ast.Ident("Internal.array_get"),[
                                           Ast.Ident(name);
                                           Ast.Constant(Ast.Int(i+1))])) args) e in
                Ast.Case(rw_cst env c,e')
             | Past.Otherwise e -> Ast.Otherwise(rw_exp env e)) ms))

and rw_cst env = function
  | Past.Unit -> Ast.Unit
  | Past.Bool b -> Ast.Bool b
  | Past.Int n -> Ast.Int n 
  | Past.Char c -> Ast.Char c 
  | Past.String s -> Ast.String s
  | Past.Constr name -> let id,_ = assoc_constructor name env in Ast.Int(id)
  | Past.Array_empty -> Ast.Array_empty 
and visit_binop = function
  | Past.Add -> Ast.Add
  | Past.Minus -> Ast.Minus
  | Past.Mult -> Ast.Mult
  | Past.Div -> Ast.Div
  | Past.Eq -> Ast.Eq
  | Past.Neq -> Ast.Neq
  | Past.Gt -> Ast.Gt
  | Past.Ge -> Ast.Ge
  | Past.Lt -> Ast.Lt
  | Past.Le -> Ast.Le
  | Past.Or -> Ast.Or
  | Past.And -> Ast.And
  | Past.Lor -> Ast.Lor
  | Past.Land -> Ast.Land
and visit_unop = function
  | Past.UMinus -> Ast.UMinus
