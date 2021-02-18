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


open Ast
open Types

(* algorithme W *)
let rec w e = 
  try 
    Printf.printf "- : %s\n" (Types.string_of_typ (w_exp empty_env e))
  with
 | UnificationFailure (t1,t2) -> 
      Printf.printf "\nError: This expression has type %s but an expression was expected of type %s\n" (Types.string_of_typ t1) (Types.string_of_typ t2); exit 0
  | Unbound_value (x) -> Printf.printf "Error: Unbound value %s\n" x; exit 0
  | exn -> Printf.printf "UN BUG DANS LE TYPEUR. on continue : %s\n"
             ( Printexc.to_string exn)

and w_exp env e = 
match e with
  | Int n -> Tint
  | Var x -> find x env
  | Let (x,e1,e2) ->
      let tx = w_exp env e1 in
      w_exp (add true x tx env) e2
  | Letrec (x,e1,e2) -> 
  let tx = Tvar (V.create ()) in
  let env1 = add true x tx env in
  let t1 = w_exp env1 e1 in
  unify tx t1;
  w_exp env1 e2
  | Lam (x,e1) ->
      let tx = Tvar (V.create ()) in
      let env = add false x tx env in
      let t1 = w_exp env e1 in
      Tarrow (tx, t1)
  | App (e1, e2) ->
      let t1 = w_exp env e1 in
      let t2 = w_exp env e2 in
      let v = Tvar (V.create ()) in
      unify t1 (Tarrow (t2,v));
      v
  | Add (e1,e2) | Sub (e1,e2) ->
    let t1 = w_exp env e1 in
    let t2 = w_exp env e2 in
    unify t1 Tint;
    unify t2 Tint;
    Tint
  | IfZero (e1, e2, e3) -> 
      let t1 = w_exp env e1 in
      let t2 = w_exp env e2 in
      let t3 = w_exp env e3 in
      unify t1 Tint;
      unify t2 t3;
      t2
