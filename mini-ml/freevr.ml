(**************************************************************************)
(*                                                                        *)
(*         PSTL : OCaml sur la plate-forme Nand2Tetris (2020)             *)
(*                                                                        *)      
(*           Loïc SYLVESTRE              Pablito BELLO                    *)
(*           loic.sylvestre@etu.upmc.fr  pablito.bello@etu.upmc.fr        *)
(*                                                                        *)  
(**************************************************************************)

type collect = string list 

let create () = ref []

let rec collect_exp coll env lenv e =
match e with 
  | Ast.Constant (_) -> ()
  | Ast.Ident(name) -> 
    if (not (List.exists (String.equal name) lenv))
       && List.exists (String.equal name) env
    then coll := name :: !coll
| Ast.Let(name,e1,e2) -> 
     let lenv' = name :: lenv in 
     collect_exp coll env lenv e1;
     collect_exp coll env lenv' e2 
| Ast.LetRec(name,e1,e2) -> 
     let lenv' = name :: lenv in 
     collect_exp coll env lenv' e1;
     collect_exp coll env lenv' e2 
 | Ast.Fun(name,e) ->
     let lenv = name :: lenv in 
     collect_exp coll env lenv e
| Ast.BinOp(op,e1,e2) ->
     collect_exp coll env lenv e1;
     collect_exp coll env lenv e2
  | Ast.UnOp(op,e1) ->
     collect_exp coll env lenv e1
  | Ast.App(e,args) ->
     collect_exp coll env lenv e;
     List.iter (collect_exp coll env lenv) args
  | Ast.If(e1,e2,e3) ->
     collect_exp coll env lenv e1;
     collect_exp coll env lenv e2;
     collect_exp coll env lenv e3
  | Ast.Block(es) ->
     List.iter (collect_exp coll env lenv) es
  | Ast.Seq(e1,e2) ->
     collect_exp coll env lenv e1;
     collect_exp coll env lenv e2
  | Ast.While(e1,e2) ->
     collect_exp coll env lenv e1;
     collect_exp coll env lenv e2
  | Ast.Match(e,cases) ->
     collect_exp coll env lenv e ;
      let match_case x = 
        let e = (match x with 
                 | Ast.Case (_,e) -> e 
                 | Ast.Otherwise (e) -> e)
        in 
        collect_exp coll env lenv e in
     (List.iter match_case cases)
  | _ -> ()


let collect env lenv e = (* "env" est l'environnement local englobant *)
  match env with 
  | [] -> []
  (* "la variable libre de quelqu'un et la variable liée de quelqu'un d'autre" *)
  | _ -> let coll = create () in 
         collect_exp coll env lenv e;
         !coll

(*
let _ = 
  let l = Ast.Let("bar",Ast.Constant(Ast.Int(1)),Ast.Constant(Ast.Ident("foo"))) in
  List.iter (fun x -> print_string x) (collect ("foo" :: []) [] l)
*)
