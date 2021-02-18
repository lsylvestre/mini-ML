(**************************************************************************)
(*                                                                        *)
(*         PSTL : OCaml sur la plate-forme Nand2Tetris (2020)             *)
(*                                                                        *)      
(*           Loïc SYLVESTRE              Pablito BELLO                    *)
(*           loic.sylvestre@etu.upmc.fr  pablito.bello@etu.upmc.fr        *)
(*                                                                        *)  
(**************************************************************************)

open Ast
open Bc

let apply_code = ref []
let codes = ref []
let label = ref 0

let next_label () = incr label; !label

let nb_locals bc = 
  List.fold_left (fun m a -> match a with Push(Local(n)) -> max (n+1) m | _ -> m)  0 bc

let rec compile_exp e env n =
  match e with
  | Var x -> (match List.assoc_opt x env with
              | None -> failwith ("Error: Unbound value " ^ x)
              | Some bc -> bc)
  | Let (x,e1,e2) -> (let env2 = (x,[Push(Local n)]) :: env	
                      and n2 = n + 1 in
                      compile_exp e1 env n @ 
                        [Pop(Local n)] @
                          compile_exp e2 env2 n2)
  | Letrec (x,lam,e) -> 
     (let env2 = (x,[Push(Local n)]) :: env
      and n2 = n + 1 in
      [Push(Constant 0);Pop(Local(n))] @
	compile_exp lam env2 n2 @ 
	  [Pop(Local(n));
	   Push(Local(n));
	   Push(Constant(1));
	   Push(Local(n));
	   Call("ML_Internal.array_set",3)] @
	    compile_exp e env2 n2)
  | App(e1,e2) ->
     compile_exp e1 env n @ 
       compile_exp e2 env n @ 
         [Call("Main.apply",2)]
  | Lam(x,e) -> 
     let l = next_label () in
     let len = List.length env in 
     let () = 
       let name = "Main.lambda" ^ string_of_int l in
       let case = [Push(Argument(0));
                   Push(Constant(0));
                   Call("ML_Internal.array_get",2);
                   Push(Constant(l));
                   Op(Eq);
                   Op(Not);
                   IfGoto(l);
                   Push(Argument(0));
                   Push(Argument(1));
                   Call(name,2);
                   Goto(0);
                   Label(l)] in
       apply_code := case @ !apply_code;
       let env' = (x,[Push(Argument(1))]) :: 
      	            List.mapi (fun i (x,_) -> 
      	            	(x,[Push(Argument(0)); 
      	            	    Push(Constant(i+1));
      	            	    Call("ML_Internal.array_get",2)])) env in
       let bc = compile_exp e env' n in
       codes := [Function(name,nb_locals bc)]
                @ bc @ [Return] @ !codes 
     in
     [Push(Constant (len+1));
      Call("ML_Internal.array_create_uninitialized",1);
      Pop(Local(n));
      Push(Local(n));
      Push(Constant(0));
      Push(Constant(l));
      Call("ML_Internal.array_set",3)] @
       List.concat (List.mapi (fun i (v,c) ->
     	                [Push(Local(n));
                         Push(Constant(i+1))] @
                          c @ 
                            [Call("ML_Internal.array_set",3)]) env)
       @ [Push(Local(n))]

  | IfZero(e1,e2,e3) -> 
     let lnotzero = next_label () in
     let lend = next_label () in
     compile_exp e1 env n @ 
       [IfGoto(lnotzero)] @
         compile_exp e2 env n @ 
           [Goto(lend);
            Label(lnotzero)] @
             compile_exp e3 env n @ 
               [Label(lend)]

  | Int n -> [Push(Constant(n))]
  | Add(e1,e2) -> compile_exp e1 env n @ 
                    compile_exp e2 env n @ [Op(Add)]
  | Sub(e1,e2) -> compile_exp e1 env n @ 
                    compile_exp e2 env n @ [Op(Sub)]

let compile e =
  apply_code := [];
  codes := [];
  label := 0;
  let bc = compile_exp e [] 0 in
  (match !apply_code with 
   | [] -> [] 
   | cs -> [Function("Main.apply",2)] @ cs @ [Label(0);Return]) @
    !codes @ 
      [Function("Main.main",nb_locals bc)] @
	bc @ 
	  [Call ("ML_Internal.print_int",1);Return]

let main p =
  let bc = compile p in
  String.concat "\n" (string_of_instrs bc)


