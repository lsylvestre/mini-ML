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

type typid = string

type typ =
  | Tint
  | Tbool
  | Tunit
  | Tchar
  | Tstring
  | Tvar of tvar
  | Tarrow of typ * typ
  | Tproduct of typ * typ
  | Tarray of typ
  | Tref of typ
  | Tconstr of (typid * typ list)
  | Tident of typid
and tvar =
  { id : int;
    mutable def : typ option }

let alias = ref ([] : (typid * typ) list)
(* alias définis dans le module courrant *)
let old_alias = ref ([] : (typid * typ) list)
(* alias, de la forme M.t, définis dans de précédents modules *)

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
  | Tvar _ | Tident _ | Tint | Tbool | Tunit | Tchar | Tstring as t -> t
  | Tarrow (t1, t2) -> Tarrow (canon t1, canon t2)
  | Tproduct (t1, t2) -> Tproduct (canon t1, canon t2)
  | Tarray t -> Tarray (canon t)
  | Tref t -> Tref (canon t)
  | Tconstr (c,ts) -> Tconstr (c,List.map canon ts)

(* unification *)

exception UnificationFailure of typ * typ * Parseutils.pos

exception Alias_not_found of string * Parseutils.pos

let unification_error t1 t2 loc = raise (UnificationFailure (canon t1, canon t2, loc))

let rec occur v t = match head t with
  | Tvar w -> V.equal v w
  | Tarrow (t1, t2) | Tproduct (t1, t2) -> occur v t1 || occur v t2
  | Tarray t1 | Tref t1 -> occur v t1
  | Tconstr (_,ts) -> List.exists (occur v) ts
  | Tident _ | Tunit | Tint | Tbool | Tchar | Tstring -> false

let rec unify t1 t2 loc = 
  match head t1, head t2 with
  | Tunit, Tunit -> ()
  | Tint, Tint -> ()
  | Tbool, Tbool -> ()
  | Tchar, Tchar -> ()
  | Tstring, Tstring -> ()
  (* | Tident s, Tident s' -> if s <> s' then unification_error t1 t2 *)                      
  | Tvar v1, Tvar v2 when V.equal v1 v2 -> ()
  | Tvar v1 as t1, t2 ->
      if occur v1 t2 then unification_error t1 t2 loc;
      assert (v1.def = None);
      v1.def <- Some t2
  | t1, Tvar v2 ->
      unify t2 t1 loc
  | Tarrow (t11, t12), Tarrow (t21, t22)
  | Tproduct (t11, t12), Tproduct (t21, t22) ->
      unify t11 t21 loc; unify t12 t22 loc
  | Tarray t, Tarray (t') | Tref t, Tref (t')  -> 
      unify t t' loc
  | Tconstr (c,ts), Tconstr (c',ts') -> if c <> c' then unification_error t1 t2 loc else
                                      List.iter2 (fun t1 t2 -> unify t1 t2 loc) ts ts' 
  | Tident s, ty -> unify (match List.assoc_opt s !alias with
                           | Some t -> t 
                           | None -> (match List.assoc_opt s !old_alias with
                                     | Some t -> t 
                                     | None -> raise (Alias_not_found (s,loc)))) ty loc
  | ty, Tident s -> unify t2 t1 loc
  | t1, t2 -> 
      unification_error t1 t2 loc

let cant_unify ty1 ty2 =
  try let _ = unify ty1 ty2 in false with UnificationFailure _ -> true


(* schéma de type *)

module Vset = Set.Make(V)

type schema = { vars : Vset.t; typ : typ }

(* variables libres *)

let rec fvars t = match head t with
  | Tunit | Tint | Tbool | Tchar | Tstring | Tident _ -> Vset.empty
  | Tarrow (t1, t2) | Tproduct (t1, t2) -> Vset.union (fvars t1) (fvars t2)
  | Tarray t | Tref t -> (fvars t)
  | Tconstr (c,ts) ->  List.fold_left (fun acc t -> Vset.union acc (fvars t)) Vset.empty ts
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

exception Unbound_value of string * Parseutils.pos

(* find x loc env donne une instance fraîche de env(x) *)
let find x loc env =
  let tx = 
    try Smap.find x env.bindings with 
    | Not_found -> raise (Unbound_value (x,loc)) in
  let s =
    Vset.fold (fun v s -> Vmap.add v (Tvar (V.create ())) s)
      tx.vars Vmap.empty
  in
  let rec subst t = match head t with
    | Tvar x as t -> (try Vmap.find x s with Not_found -> t)
    | Tunit -> Tunit
    | Tint -> Tint
    | Tbool -> Tbool
    | Tchar -> Tchar
    | Tstring -> Tstring
    | Tident s -> Tident s
    | Tarrow (t1, t2) -> Tarrow (subst t1, subst t2)
    | Tproduct (t1, t2) -> Tproduct (subst t1, subst t2)
    | Tarray t -> Tarray (subst t)
    | Tref t -> Tref (subst t)
    | Tconstr (c,ts) -> Tconstr (c,List.map subst ts)
  in
  subst tx.typ


