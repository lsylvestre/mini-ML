(**************************************************************************)
(*                                                                        *)
(*         PSTL : OCaml sur la plate-forme Nand2Tetris (2020)             *)
(*                                                                        *)      
(*           Loïc SYLVESTRE              Pablito BELLO                    *)
(*           loic.sylvestre@etu.upmc.fr  pablito.bello@etu.upmc.fr        *)
(*                                                                        *)  
(**************************************************************************)


let mapcat f l = List.concat (List.map f l)
let rev_mapcat f l = List.concat (List.rev_map f l)

let nb_local bc =
  List.fold_left
    (fun n -> fun u -> match u with
                       | Bc.Pop(seg) ->
                          (match seg with
                             Bc.Local(i) -> Pervasives.max (i+1) n | _ -> n)
                       | _ -> n) 0 bc

let gen_label = Gensym.create 0

let lambda_code = ref []

let apply_code = ref (Bc.Label "EndApply" :: 
                        Bc.Return :: 
                          [])


let next_closure bc_e adr = 
  let l = ((^) "A" (string_of_int adr)) in
  let f = ((^) "Apply.closure" (string_of_int adr)) in
  lambda_code := (Bc.Function (f,nb_local bc_e) :: [])
                 @ bc_e @ (Bc.Return :: []) @ (!lambda_code);
  apply_code :=  Bc.Push(Bc.Argument(0)) ::   (* arg0 : [|code;addr;vars...|] *)
                   Bc.Push(Bc.Constant(0)) ::  
                     Bc.Call("Internal.array_get",2) ::
                       Bc.Push(Bc.Constant(adr)) ::
                         Bc.BinOp(Bc.Eq) ::
                           Bc.UnOp(Bc.Not) ::    (* si (pointeur de code /= adr) *)   
                             Bc.IfGoto l ::         (* chercher à l'adresse suivante *)
                               (* Push(Argument(1)); *)
                               (* sinon, appele le code à l'adresse adr, 
                      en lui passant le paramètre effectif (arg1) 
                      et l'environnement (arg0)) *) 
                               Bc.Push(Bc.Argument(0)) ::
                                 (* la raison pour laquelle l'environnement est passé en 2ème 
                        (pas en premier)  est que le code de la fermeture suppose 
                        que l'argument est en position 0 (voir ast2kast.ml) *)
                                 Bc.Push(Bc.Argument(1)) ::
                                   Bc.Call(f,2) ::
                                     Bc.Goto "EndApply" ::
                                       Bc.Label l :: [] @ (!apply_code)


let indent_string = "  "


let bc_of_binop op = 
  match op with
  | Ast.Add -> (Bc.BinOp Bc.Add) :: []
  | Ast.Minus -> (Bc.BinOp Bc.Sub) :: []
  | Ast.Mult -> (Bc.BinOp Bc.Mult) :: []
  | Ast.Div -> (Bc.BinOp Bc.Div) :: []
  | Ast.Eq -> (Bc.BinOp Bc.Eq) :: [] 
  | Ast.Neq -> (Bc.BinOp Bc.Eq) :: (Bc.UnOp Bc.Not)  :: []
  | Ast.Gt -> (Bc.BinOp Bc.Gt) :: []
  | Ast.Lt -> (Bc.BinOp Bc.Lt) :: []
  | Ast.Le -> (Bc.BinOp Bc.Gt) :: (Bc.UnOp Bc.Not) :: []
  | Ast.Ge -> (Bc.BinOp Bc.Lt) :: (Bc.UnOp Bc.Not) :: []
  | Ast.And -> (Bc.BinOp Bc.And) :: []
  | Ast.Land -> (Bc.BinOp Bc.And) :: []
  | Ast.Or ->  (Bc.BinOp Bc.Or) :: []
  | Ast.Lor -> (Bc.BinOp Bc.Or) :: []

let bc_of_unop op = 
  match op with
  | Ast.UMinus -> (Bc.Pop (Bc.Temp(0))) ::
                    (Bc.Push(Bc.Constant(0))) :: 
                      (Bc.Push(Bc.Temp(0))) ::
                        (Bc.BinOp Bc.Sub) :: []


let bc_of_constant c = 
  match c with
  | Kast.Unit -> (Bc.Push (Bc.Constant 0)) :: []
  | Kast.Int(n) -> (Bc.Push (Bc.Constant n)) :: []
  (* if n >= 0
     then (Bc.Push (Bc.Constant n)) :: []
     else (Bc.Push (Bc.Constant 0)) ::
          (Bc.Push (Bc.Constant (- n))) :: 
          (Bc.BinOp(Bc.Sub)) :: [] *)
  | Kast.Array_empty ->
     (Bc.Push (Bc.Constant 0)) :: []
  | Kast.Bool(b) ->
     if b then (Bc.True) :: [] else (Bc.False) :: []

let bc_of_variable v = match v with
  | Kast.Global(name) -> 
     (Bc.Call (name,0)) :: []
  (* les variables globales sont des fonction globales d'arité 0  *) 
  (* avec une étape d'initialisation au démarrage du programme.   *)
  (* --> la fonction name est un simple "getter"                  *)
  (* NB : pas de recalcule de l'expression à chaque accès.        *)
  | Kast.Argument (n) ->
     (Bc.Push(Bc.Argument n)) :: []
  | Kast.Local (n) ->
     (Bc.Push(Bc.Local n)) :: []
  | Kast.Free (n) ->
     (Bc.Push(Bc.Argument 0)) ::
       (Bc.Push(Bc.Constant (n))) ::
         (Bc.Call("Internal.array_get",2)) :: []
(* la nieme valeur de l'env, (sans compter l'addresse en position 0) *)


let rec bc_of_exp e =
  match e with 
  | Kast.Constant(c) -> bc_of_constant c
  | Kast.Variable(v) -> bc_of_variable v
  | Kast.If(e1,e2,e3) ->
     let bc_e1 = bc_of_exp e1 in
     let bc_e2 = bc_of_exp e2 in
     let bc_e3 = bc_of_exp e3 in
     let lbl_if_true = Gensym.next "IfTrue" gen_label in
     let lbl_end = Gensym.next "IfEnd" gen_label in
     bc_e1 @ ((Bc.IfGoto lbl_if_true) :: []) @
       bc_e3 @ ((Bc.Goto lbl_end) ::
                  (Bc.Label lbl_if_true) :: []) @
         bc_e2 @ ((Bc.Label lbl_end) :: [])
  | Kast.While(e1,e2) ->
     let bc_e1 = bc_of_exp e1 in 
     let bc_e2 = bc_of_exp e2 in
     let lbl_begin = Gensym.next "WhileBegin" gen_label in
     let lbl_end = Gensym.next "WhileEnd" gen_label in
     ((Bc.Label lbl_begin) :: []) @ 
       bc_e1 @ 
         ((Bc.UnOp Bc.Not) :: (Bc.IfGoto lbl_end) :: []) @
           bc_e2 @ 
             ((Bc.Goto lbl_begin) :: (Bc.Label lbl_end) :: [])
  | Kast.Closure (code,closure_env) ->
     let adr = fst code in
     let ke = snd code in
     (* let n = next_lambda (bc_of_exp lvl e) *)
     next_closure (bc_of_exp ke) adr;
     bc_of_exp closure_env 
  (* la fermeture est un tableau (module Array)) *)
  (* dont le premier élément est l'id (~+/- l'adresse) de la closure *) 
  (* et les élément suivant sont les "variables libres" (l'environnement). *)
  (* Les fonctions sont unaires : on les utilises avec Apply.apply (code autogénéré) *)
  (* en passant en [argument 0] la fermeture elle même et en [argument 1] son argument. *)
  (* NB : l'argument de la fonction ne fait pas parti de l'environnement stocké dans la fermeture. *)
  | Kast.Let(n,e1,e2) ->
     let bc_e1 = bc_of_exp e1 in
     let bc_e2 = bc_of_exp e2 in
     bc_e1 @ ((Bc.Pop(Bc.Local n)) :: []) @ bc_e2
  | Kast.Seq(e1,e2) ->
     let bc_e1 = bc_of_exp e1 in
     let bc_e2 = bc_of_exp e2 in
     bc_e1 @ ((Bc.Pop Bc.Anywhere) :: []) @ bc_e2
  | Kast.App(e,args) -> 
     (match e with
      (* appel de fonctions globales *)
      | Kast.GFun (name) -> let arity = List.length args in 
                            mapcat bc_of_exp args @ ((Bc.Call(name,arity)) :: [])
      (* mécanisme générale : application de fonctions unaires *)
      | _ -> (* calcul de l'expression en position fonctionnelle *)
         (let eB = bc_of_exp e 
          in (* puis calcul des arguments (contrairement à OCAML) *)
          let argsB = mapcat (fun e -> (bc_of_exp e) @ (((Bc.Call("Apply.apply",2))) :: [])) args in         
          eB @ argsB))
  (* C[(e0 e1 ... ek)]            *)
  (* ~> C[(e0 e1 ... e(k-1))];    *)  
  (*    C[ek];                    *)
  (*    Call(Apply.apply,2);      *) 

  (* en encore :                  *)
  (* ~> C[(e0)]                   *)  
  (*    C[(e1)]                   *) 
  (*    Call(Apply.apply,2);      *) 
  (*    C[(e2)]                   *)  
  (*    Call(Apply.apply,2);      *) 
  (*    ...                       *)
  (*    C[(ek)]                   *)  
  (*    Call(Apply.apply,2);      *) 
  | Kast.BinOp(op,e1,e2) ->
     let bc_e1 = bc_of_exp e1 in
     let bc_e2 = bc_of_exp e2 in
     bc_e1 @ bc_e2 @ bc_of_binop op
  | Kast.UnOp(op,e1) ->
     let bc_e1 = bc_of_exp e1  in
     bc_e1 @ bc_of_unop op
  | Kast.GFun (name) -> (Bc.Call (name,0)) :: [] (* assert false  *)
  (* Comme il n'y a pas d'adresse en Nand2Tetris, une fonction globale n'a pas de  *)
  (* valeur (même pas un pointeur). *)

  | Kast.Ext(ext) -> 
     (match ext with 
      | Kast.SetGlobal (e1,i) ->
         let bc_e1 = bc_of_exp e1 in
         bc_e1 @ ((Bc.Pop (Bc.Static(i))) :: []) @ 
           ((Bc.Push (Bc.Static(i))) :: (Bc.Pop (Bc.Temp(7))) :: [])
      | Kast.ReadGlobal (i) -> 
         (Bc.Push (Bc.Static(i))) :: []
      | Kast.SetLocal(n,e) -> 
         (bc_of_exp e) @ ((Bc.Pop (Bc.Local(n))) :: [])
      | Kast.Label (s,e) -> 
         (Bc.Label(s)) :: (bc_of_exp e)
      | Kast.Goto (s,args) -> 
         let xs = mapcat bc_of_exp (List.rev args) in
         let m = List.mapi (fun i -> fun _ -> Bc.Pop (Bc.Argument(i))) args in
         xs @ (m @ (((Bc.Goto(s))) :: [])))

let bc_of_decl mod_name d = 
  match d with 
  | Kast.DefFun (name,arity,e) ->
     let bc_e = bc_of_exp e in
     let full_name = (^) ((^) mod_name ".") name in
     (Bc.Function (full_name,nb_local bc_e)) :: bc_e @ ((Bc.Return) :: [])

let bc_of_decls mod_name ds = 
  mapcat (bc_of_decl mod_name) ds


let bc_of_tmodule init m =
  match m with
  | Kast.Module (mod_name,decls,_) -> 
     let bc_body = bc_of_decls mod_name decls in
     Bc.Module (mod_name,bc_body,init)


let bc_of_prog bc_mdls =
  let accgb = ref [] in
  let files =
    List.map (fun m -> match m with 
                         Bc.Module (mod_name,bc_body,init) ->
                         accgb := init @ (!accgb); 
                         (mod_name,bc_body)) bc_mdls in
  let init_globals = List.rev (mapcat (fun g -> (Bc.Call (g,0) :: 
                                                   [])) (!accgb)) in
  let main = ("Start",(Bc.Function ("Start.main",0) ::
                         []) @ init_globals @ 
                        (Bc.Push(Bc.Constant(0)) ::
                           Bc.Return :: [])
             ) in
  let apply_file = ("Apply",(Bc.Function ("Apply.apply",0) :: !apply_code) @ !lambda_code) in

  apply_file :: main :: files
