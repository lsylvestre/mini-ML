(**************************************************************************)
(*                                                                        *)
(*         PSTL : OCaml sur la plate-forme Nand2Tetris (2020)             *)
(*                                                                        *)      
(*           Loïc SYLVESTRE              Pablito BELLO                    *)
(*           loic.sylvestre@etu.upmc.fr  pablito.bello@etu.upmc.fr        *)
(*                                                                        *)  
(**************************************************************************)

open Past

let sptf = Printf.sprintf

let next n = n + 2

let indent_string (level:int) : string =
  String.make level ' '

let mapcat c f l = String.concat c (List.map f l)

let get_indent_level s lvl =
  let lvl' = match String.rindex_opt s '\n' with
    | None -> lvl + String.length s
    | Some i -> String.length s - i in lvl' + 1

let rec sprint_prog ms =
  List.map (sprint_module 0) ms
and sprint_module lvl {mod_name;decls} =
  sptf "%smodule %s = struct\n%s ;;\n%send\n\n"
    (indent_string lvl)
    mod_name 
    (mapcat " ;;\n\n" (sprint_decl (next lvl)) decls)
    (indent_string lvl) 
and sprint_decl lvl {decl_desc} = match decl_desc with
  | Type (name,largsv,ty) ->
     let sty = match ty with 
       | Exp_ty t -> (sprint_ty lvl t)
       | Sum (l) ->
          mapcat " | "
            (function
               (c,[]) -> c
             | (c,args) ->
                c ^ " of " ^ (mapcat " * " (sprint_ty 0) args)) l
     in
     sptf "%stype %s%s = %s"
       (indent_string lvl)
       (match largsv with 
        | [] -> ""
        | _ -> " (" ^ (mapcat ", " (sptf "'%s") largsv) ^ ") ") name sty
  | DefVar (var,e) ->
     let w = sptf "let %s = " (sprint_var lvl var) in
     let z = get_indent_level w lvl in
     sptf "%s%s" w
       (sprint_exp z e)
  | DefFun (dfs) ->
     sprint_fun lvl dfs
  | DefFunRec (dfs) ->
     sprint_fun ~recflag:true lvl dfs
and sprint_fun ?(recflag=false) lvl l =
  (indent_string lvl) ^
    (if recflag then "let rec" else "let") ^
      String.concat ("\n" ^ indent_string lvl ^ "and") @@
        List.map (fun (name,args,tyopt,e) ->
            let s = sptf " %s %s %s= " name (mapcat " " (sprint_var lvl) args) 
                      (match tyopt with 
                       | None -> ""
                       | Some ty -> sptf ": %s " (sprint_ty lvl ty)) in
            s ^ "\n" ^ (indent_string (next lvl)) ^ (sprint_exp (next lvl) e)) l
and sprint_exp lvl {exp_desc} = match exp_desc with
  | Annotation (e,ty) ->
     sptf "(%s : %s)"
       (sprint_exp (lvl+1) e)
       (sprint_ty (lvl+1) ty)
  | Constant(c) ->
     sprint_constant lvl c
  | Ident name -> name
  | Let(var,e1,e2) ->
     let w = sptf "(let %s = " (sprint_var lvl var) in
     let z = get_indent_level w lvl in
     sptf "%s%s in\n%s%s)" w
       (sprint_exp (z) e1)
       (indent_string lvl)
       (sprint_exp lvl e2)
  | LetRec(name,e1,e2) ->
     let w = sptf "(let rec %s = " name in
     let z = get_indent_level w lvl in
     sptf "%s%s in\n%s%s)" w
       (sprint_exp (z) e1)
       (indent_string lvl)
       (sprint_exp lvl e2)
  | Fun(var,e) ->
     let w = sptf "(fun %s -> " (sprint_var lvl var) in
     let z = get_indent_level w lvl in
     sptf "%s%s)" w (sprint_exp z e)
  | BinOp(op,e1,e2) ->
     let lvl = lvl + 1 in (* pour la parenthèse ouvrante *)
     let s = "(" ^ sprint_exp lvl e1 in
     let lvl' = get_indent_level s lvl in
     let ops = " " ^ sprint_binop lvl' op ^ " " in
     let opz = get_indent_level ops lvl in
     s ^ ops ^ (sprint_exp (lvl'+opz) e2) ^ ")"
  | UnOp(op,e1) ->
     let lvl = lvl + 1 in (* pour la parenthèse ouvrante *)
     let s = "(" ^ sprint_unop lvl op ^ " " in
     let lvl' = get_indent_level s lvl in
     sptf "%s%s)" s (sprint_exp lvl' e1)
  | App(e,args) ->
     let s = sptf "(%s " (sprint_exp lvl e) in
     let lvl' = get_indent_level s lvl in 
     s ^ (mapcat " " (sprint_exp lvl') args) ^ ")"    
  (* à revoir, indentation pas terrible si plusieurs "gros" arguments *)
  | If(e1,e2,e3) ->
     let lvl = lvl + 1 in (* pour la parenthèse ouvrante *)
     sptf "(if %s\n%sthen %s \n%selse %s)"
       (sprint_exp lvl e1)
       (indent_string lvl) (sprint_exp lvl e2)
       (indent_string lvl) (sprint_exp lvl e3)
  | Ref(e) ->
     sptf "(ref %s)" (sprint_exp 0 e)
  | Ref_access(e1) ->
     sptf "(! %s)" (sprint_exp 0 e1)
  | Ref_assign(e1,e2) ->
     sptf "(%s := %s)" (sprint_exp 0 e1) (sprint_exp 0 e2)
  | Pair(e1,e2) ->
     sptf "(%s, %s)" (sprint_exp 0 e1) (sprint_exp 0 e2) 
  | Array_create(es) ->
     "[|" ^ mapcat "; " (sprint_exp 0) es ^ "|]"
  | Array_assign(e1,e2,e3) ->
     sptf "((%s).(%s) <- %s)" 
       (sprint_exp lvl e1)
       (sprint_exp (next lvl) e2)
       (sprint_exp (next lvl) e3)
  | Array_access(e1,e2) ->
     sptf "%s.(%s)"
       (sprint_exp 0 e1)
       (sprint_exp 0 e2)
  | Seq(e1,e2) ->
     sptf "(%s;\n%s%s)"
       (sprint_exp lvl e1)
       (indent_string lvl)
       (sprint_exp (lvl) e2)
  | While(e1,e2) ->
     sptf "while %s do\n%s\n%sdone"
       (sprint_exp lvl e1)
       (sprint_exp (lvl+1) e2)
       (indent_string lvl)
  | For(x,e1,e2,e3) ->
     sptf "for %s = %s to %s do\n%s\n%sdone"
       x
       (sprint_exp 0 e1)
       (sprint_exp lvl e2)
       (sprint_exp (next lvl) e3) 
       (indent_string lvl)
  | Assert(e,_) ->
     let s = sptf "(assert " in
     let lvl' = get_indent_level s lvl in 
     s ^ (sprint_exp lvl' e) ^ ")"
  | Match(e1,cases) ->
     let lvl = lvl + 1 in (* pour la parenthèse ouvrante *)
     sptf "(match %s with\n%s"
       (sprint_exp 0 e1)
       (String.concat "\n"
          (List.map 
             (function 
              | Case (c,args,e) ->
                 let s = sptf "%s| %s%s -> "
                           (indent_string lvl) (sprint_constant lvl c)
                           (match args with 
                            | [] -> ""
                            | _ -> "(" ^ String.concat ", " args ^")")  in
                 let lvl' = get_indent_level s 0 in
                 s ^ (sprint_exp lvl' e)
              | Otherwise (e) ->
                 let s = sptf "%s| _ -> "
                           (indent_string lvl) in
                 let lvl' = get_indent_level s lvl in
                 s ^ (sprint_exp lvl' e))
             cases)) ^ ")"
  | Magic(e) -> sptf "(Obj.magic %s)" (sprint_exp 0 e)
and sprint_constant lvl = function
  | Unit -> sptf "()"
  | Bool b -> sptf "%b" b
  | Int n -> sptf (if n >= 0 then "%d" else "(%d)") n
  | Char c -> sptf "%c" c
  | String s -> sptf "\"%s\"" s
  | Constr name -> name
  | Array_empty -> "[||]"
and sprint_binop lvl = function
  | Add -> "+"
  | Minus -> "-"
  | Mult -> "*"
  | Div -> "/"
  | Eq -> "="
  | Neq -> "<>"
  | Gt -> ">"
  | Ge -> ">="
  | Lt -> "<"
  | Le -> "<="
  | Or -> "||"
  | And -> "&&"
  | Lor -> "lor"
  | Land -> "land"
and sprint_unop lvl = function
  | UMinus -> "-"  
and sprint_ty lvl ty = 
  match ty with
  (*| Sum (names) -> String.concat " | " names *)
  | Tint -> "int"
  | Tbool -> "bool"
  | Tchar -> "char"
  | Tunit -> "unit"
  | Tstring -> "string"
  | Tident (name) -> name 
  | Tproduct (t1,t2) ->
     sptf "(%s * %s)"
       (sprint_ty lvl t1)
       (sprint_ty lvl t2)
  | Tarrow (t1,t2) ->
     sptf "(%s -> %s)"
       (sprint_ty lvl t1)
       (sprint_ty lvl t2)
  | Tarray t ->
     sptf "(%s array)"
       (sprint_ty lvl t)
  | Tref t ->
     sptf "(%s ref)"
       (sprint_ty lvl t)
  | Tvar name -> (indent_string lvl) ^ name
  | Tconstr (name,args) ->
     sptf "%s%s" 
       (match args with
        | [] -> "" 
        | [name] -> (sprint_ty lvl name) ^ " "
        | _ -> mapcat " " (sprint_ty lvl) args ^" ") name (* à revoir *)
  
and sprint_var lvl (p,opt) = 
  match opt with 
  | None -> p
  | Some ty ->
     sptf "(%s : %s)"
       p
       (sprint_ty lvl ty)






let rec sprint_real_ty lvl ty = 
  let open Types in 
  match ty with
  (*| Sum (names) -> String.concat " | " names *)
  | Tint -> "int"
  | Tbool -> "bool"
  | Tchar -> "char"
  | Tunit -> "unit"
  | Tstring -> "string"
  | Tident (name) -> name 
  | Tproduct (t1,t2) ->
     sptf "(%s * %s)"
       (sprint_real_ty lvl t1)
       (sprint_real_ty lvl t2)
  | Tarrow (t1,t2) ->
     sptf "(%s -> %s)"
       (sprint_real_ty lvl t1)
       (sprint_real_ty lvl t2)
  | Tarray t ->
     sptf "(%s array)"
       (sprint_real_ty lvl t)
  | Tref t ->
     sptf "(%s ref)"
       (sprint_real_ty lvl t)
  | Tvar v ->
     (match v.def with 
      | None -> Printf.sprintf "'a%d" v.id
      | Some ty -> sprint_real_ty lvl ty)
  | Tconstr (name,args) ->
     sptf "%s%s" 
       (match args with
        | [] -> "" 
        | [name] -> (sprint_real_ty lvl name) ^ " "
        | _ -> mapcat " " (sprint_real_ty lvl) args ^" ") name (* à revoir *)
