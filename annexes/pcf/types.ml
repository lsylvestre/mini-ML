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

type typ =
  | Tint
  | Tvar of tvar
  | Tarrow of typ * typ
and tvar = { id : int; mutable def : typ option }

(* module V pour les variables de type *)

module V = struct
  type t = tvar
  let compare v1 v2 = Pervasives.compare v1.id v2.id
  let equal v1 v2 = v1.id = v2.id
  let create = let r = ref 0 in fun () -> incr r; { id = !r; def = None }
end

let var_create () = Tvar (V.create ())

(* réduction en tête d'un type (la compression de chemin serait possible) *)
let rec head = function
  | Tvar { def = Some t } -> head t
  | t -> t

(* forme canonique d'un type = on applique head récursivement *)
let rec canon t = match head t with
  | Tvar _ | Tint as t -> t
  | Tarrow (t1, t2) -> Tarrow (canon t1, canon t2)

(* unification *)

exception UnificationFailure of typ * typ

let unification_error t1 t2 = raise (UnificationFailure (canon t1, canon t2))

let rec occur v t = match head t with
  | Tvar w -> V.equal v w
  | Tarrow (t1, t2) -> occur v t1 || occur v t2
  | Tint -> false

let rec unify t1 t2 = 
  match head t1, head t2 with
  | Tint, Tint -> ()
  | Tvar v1, Tvar v2 when V.equal v1 v2 -> ()
  | Tvar v1 as t1, t2 ->
      if occur v1 t2 then unification_error t1 t2;
      assert (v1.def = None);
      v1.def <- Some t2
  | t1, Tvar v2 ->
      unify t2 t1
  | Tarrow (t11, t12), Tarrow (t21, t22) -> 
    unify t11 t21; unify t12 t22
  | t1, t2 -> unification_error t1 t2

let cant_unify ty1 ty2 =
  try let _ = unify ty1 ty2 in false with UnificationFailure _ -> true


(* schéma de type *)

module Vset = Set.Make(V)

type schema = { vars : Vset.t; typ : typ }

(* variables libres *)

let rec fvars t = match head t with
  | Tint -> Vset.empty
  | Tarrow (t1, t2) -> Vset.union (fvars t1) (fvars t2)
  | Tvar v -> Vset.singleton v

let norm_varset s =
  Vset.fold (fun v s -> Vset.union (fvars (Tvar v)) s) s Vset.empty


(* environnement c'est une table bindings (string -> schema),
   et un ensemble de variables de types libres *)


module Smap = Map.Make(String)

type env = { bindings : schema Smap.t; fvars : Vset.t }

let empty_env = { bindings = Smap.empty; fvars = Vset.empty }

let add gen x t env =
  let vt = fvars t in
  let s, fvars =
    if gen then
      let env_fvars = norm_varset env.fvars in
      { vars = Vset.diff vt env_fvars; typ = t }, env.fvars
    else
      { vars = Vset.empty; typ = t }, Vset.union env.fvars vt
  in
  { bindings = Smap.add x s env.bindings; fvars = fvars }

module Vmap = Map.Make(V)

exception Unbound_value of string

(* find x loc env donne une instance fraîche de env(x) *)
let find x env =
  let tx = 
    try Smap.find x env.bindings with 
    | Not_found -> raise (Unbound_value x) in
  let s =
    Vset.fold (fun v s -> Vmap.add v (Tvar (V.create ())) s)
      tx.vars Vmap.empty
  in
  let rec subst t = match head t with
    | Tvar x as t -> (try Vmap.find x s with Not_found -> t)
    | Tint -> Tint
    | Tarrow (t1, t2) -> Tarrow (subst t1, subst t2)
  in
  subst tx.typ

let rec string_of_typ ty = 
  let rec aux ty = 
  match ty with
  | Tint -> "int"
  | Tvar {def=Some t} -> aux t
  | Tvar {def=None;id} -> Printf.sprintf "a%d" id
  | Tarrow (t1,t2) -> Printf.sprintf "(%s -> %s)" 
                        (aux t1)
                        (aux t2)
  in   
  match List.filter (function {def=None} -> true | _ -> false)
        (Vset.elements (fvars ty)) with
  | [] -> aux ty
  | l -> "forall " ^ (String.concat " " 
                         (List.map (function {id} -> 
                                      Printf.sprintf "a%d" id) 
                             l)) ^ " . " ^ aux ty
