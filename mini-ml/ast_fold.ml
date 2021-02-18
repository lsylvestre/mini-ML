(**************************************************************************)
(*                                                                        *)
(*         PSTL : OCaml sur la plate-forme Nand2Tetris (2020)             *)
(*                                                                        *)      
(*           Loïc SYLVESTRE              Pablito BELLO                    *)
(*           loic.sylvestre@etu.upmc.fr  pablito.bello@etu.upmc.fr        *)
(*                                                                        *)  
(**************************************************************************)

(* constant folding *)

(* rend le nombre d'occurence de la variable v dans l'expression e *)
let rec occ (v : string) e = match e with
  | Ast.Constant c -> 0
  | Ast.Let(name,e1,e2) -> if v = name then occ v e1 else occ v e1 + occ v e2
  | Ast.LetRec (f,e1,e2) -> if v <> f then occ v e1 + occ v e2 else 0
  | Ast.App(e,args) -> occ v e + occ_list v args
  | Ast.If(e1,e2,e3) -> occ v e1 + occ v e2 + occ v e3
  | Ast.BinOp(op,e1,e2) -> occ v e1 + occ v e2
  | Ast.UnOp(op,e1) -> occ v e1 
  | Ast.Seq(e1,e2) -> occ v e1 + occ v e2
  | Ast.While(e1,e2) -> occ v e1 + occ v e2
  | Ast.Match(e,ms) ->
     occ v e + occ_list v (List.map
                             (function
                                Ast.Case(_,e) -> e
                              | Ast.Otherwise e -> e) ms)
  | Ast.Ident name -> if name = v then 1 else 0
  | e -> 0
and occ_list v l = 
  List.fold_left (fun acc e -> acc + occ v e) 0 l



(* remplace les occurences de la variable locale v par x dans e. *)
let replace v x e = 
  let rec replace = function
    | Ast.Ident name -> if v = name then x else Ast.Ident name
    | Ast.Constant c -> Ast.Constant c
    | Ast.Let(name,e1,e2) -> let s = if v = name then e2 else replace e2 in
                             (* gestion de la capture *)
                             Ast.Let(name,replace e1,s)
    | Ast.LetRec (f,e1,e2) -> Ast.LetRec (f,(if v = f then e1 else replace e1),
                                          (if v = f then e2 else replace e2))
    | Ast.App(e,args) -> Ast.App(replace e,List.map replace args) 
    | Ast.If(e1,e2,e3) -> Ast.If(replace e1,replace e2,replace e3)
    | Ast.BinOp(op,e1,e2) -> Ast.BinOp(op,replace e1,replace e2)
    | Ast.UnOp(op,e1) -> Ast.UnOp(op,replace e1)
    | Ast.Seq(e1,e2) -> Ast.Seq(replace e1,replace e2)
    | Ast.While(e1,e2) -> Ast.While(replace e1,replace e2)
    | Ast.Match(e,ms) ->
       Ast.Match (replace e,List.map
                              (function
                                 Ast.Case(c,e) -> Ast.Case(c,replace e)
                               | Ast.Otherwise e -> Ast.Otherwise(replace e)) ms) 
    | e -> e in replace e


(* applique la propagation des constantes dans le modules *)
let rec rewrite m =
  match m with Ast.Module(mod_name,decls) ->
    let decls = List.map fold_decl decls in
    Ast.Module(mod_name,decls)


and fold_decl = function
  | Ast.DefVar(v,e) -> Ast.DefVar(v,fold_exp e)
  | Ast.DefFun(l) -> Ast.DefFun (fold_fundecs l)
  | Ast.DefFunRec(l) -> Ast.DefFunRec (fold_fundecs l)
  | d -> d 
and fold_fundecs l = 
  List.map (fun (Ast.DF (name,args,e)) -> (Ast.DF (name,args,fold_exp e))) l 
and fold_exp = function
  | Ast.Ident name -> Ast.Ident name
  | Ast.Constant c -> Ast.Constant c
  | Ast.Let(v,e1,e2) -> 
     let e1 = fold_exp e1 in
     let e2 = fold_exp e2 in
     (match e1 with 
      | Ast.While _ 
        -> Ast.Seq (e1,fold_exp @@ replace v (Ast.Constant(Ast.Unit)) e2)
      | _ ->
         (match e1,occ v e2 with
          | Ast.Ident (x),_ -> (replace v (Ast.Ident (x)) e2)
          (* on a : let y = x in e(x,y) ~> e(x), e(x) signifie e depend de x *)
          | Ast.Constant _,0 -> e2
          | Ast.Constant _,_ -> fold_exp (replace v e1 e2)
          | _ -> Ast.Let(v,e1,e2)))
  | Ast.LetRec(f,e1,e2) -> Ast.LetRec(f,e1,e2) (* TODO *)
  | Ast.App(e,args) -> Ast.App(fold_exp e,List.map fold_exp args) 
  | Ast.If(e1,e2,e3) -> (match fold_exp e1 with 
                         | Ast.Constant(Ast.Bool(b)) -> fold_exp (if b then e2 else e3)
                         | e -> Ast.If(e,fold_exp e2,fold_exp e3))
  | Ast.BinOp(op,e1,e2) -> eval_binop (op,fold_exp e1,fold_exp e2)
  | Ast.UnOp(op,e1) -> let x = fold_exp e1 in 
                       (match x with 
                        | Ast.Constant _ -> eval_unop (op,x)
                        | _ -> Ast.UnOp(op,x))
  | Ast.Block(xs) -> Ast.Block(List.map fold_exp xs)
  | Ast.Seq(e1,e2) -> Ast.Seq(fold_exp e1,fold_exp e2)
  | Ast.While(e1,e2) -> (match fold_exp e1 with 
                         | Ast.Constant(Ast.Bool(false)) -> Ast.Constant(Ast.Unit)
                         | e -> Ast.While(e,fold_exp e2))
  | Ast.Match(e,ms) ->  
     (* let rec aux acc = function
       | Ast.Case(c,e)::t -> aux (Ast.Case(c,fold_exp e)::acc) t
       | Ast.Otherwise(e)::_ -> List.rev (Ast.Otherwise(fold_exp e)::acc)
       | [] -> Printf.printf "Warning 8: this pattern-matching is not exhaustive.\n"; 
               List.rev
                 ((Ast.Otherwise
                     (Ast.App(
                          Ast.Ident("Pervasives.failwith"),
                          [Ast.Constant(Ast.String("Match_failure, exit."))]))) :: acc)
     in
     let ms = aux [] ms in
     (match fold_exp e with 
      | Ast.Constant c -> (match List.find
                                   (function
                                      Ast.Case(c',_) -> c' = c
                                    | Ast.Otherwise _    -> true) ms
                           with
                           | Ast.Case(_,e) -> e
                           | Ast.Otherwise e -> e)
      | e' -> Ast.Match(e',ms)) *)
     Ast.Match(e,ms)
  | e -> e
and eval_binop = function
  | (Ast.Add,Ast.Constant(Ast.Int(n)),Ast.Constant(Ast.Int(m))) ->
     Ast.Constant(Ast.Int((n+m) land 0xFFFF))
  | (Ast.Minus,Ast.Constant(Ast.Int(n)),Ast.Constant(Ast.Int(m))) ->
     Ast.Constant(Ast.Int((n-m) land 0xFFFF))
  | (Ast.Mult,Ast.Constant(Ast.Int(n)),Ast.Constant(Ast.Int(m))) ->
     Ast.Constant(Ast.Int((n*m) land 0xFFFF))
  | (Ast.Div,Ast.Constant(Ast.Int(n)),Ast.Constant(Ast.Int(m))) ->
     Ast.Constant(Ast.Int((n/m) land 0xFFFF))
  | (Ast.Lt,Ast.Constant(Ast.Int(n)),Ast.Constant(Ast.Int(m))) ->
     Ast.Constant(Ast.Bool(n < m))
  | (Ast.Le,Ast.Constant(Ast.Int(n)),Ast.Constant(Ast.Int(m))) ->
     Ast.Constant(Ast.Bool(n <= m))
  | (Ast.Neq,Ast.Constant(c1),Ast.Constant(c2)) ->
     Ast.Constant(Ast.Bool(c1 <> c2))
  | (Ast.Eq,Ast.Constant(c1),Ast.Constant(c2)) ->
     Ast.Constant(Ast.Bool(c1 = c2))
  | (Ast.Ge,Ast.Constant(Ast.Int(n)),Ast.Constant(Ast.Int(m))) ->
     Ast.Constant(Ast.Bool(n >= m))
  | (Ast.Gt,Ast.Constant(Ast.Int(n)),Ast.Constant(Ast.Int(m))) ->
     Ast.Constant(Ast.Bool(n > m))
  | (Ast.Or,Ast.Constant(Ast.Bool(p)),Ast.Constant(Ast.Bool(q))) ->
     Ast.Constant(Ast.Bool(p || q))
  | (Ast.And,Ast.Constant(Ast.Bool(p)),Ast.Constant(Ast.Bool(q))) ->
     Ast.Constant(Ast.Bool(p && q))
  (* land et lor dépendent de l'architecture (16 bits *)
  | (op,e1,e2) -> Ast.BinOp(op,e1,e2)
and eval_unop = function
  | (Ast.UMinus,Ast.Constant(Ast.Int(n))) ->
     Ast.Constant(Ast.Int(- n))
  | (op,e) -> Ast.UnOp(op,e)


