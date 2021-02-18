(**************************************************************************)
(*                                                                        *)
(*         PSTL : OCaml sur la plate-forme Nand2Tetris (2020)             *)
(*                                                                        *)      
(*           Loïc SYLVESTRE              Pablito BELLO                    *)
(*           loic.sylvestre@etu.upmc.fr  pablito.bello@etu.upmc.fr        *)
(*                                                                        *)  
(**************************************************************************)


open Ast

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
and sprint_module lvl m = 
  match m with 
    Module(mod_name,decls) ->
    sptf "%smodule %s = struct\n%s ;;\n%send\n\n"
      (indent_string lvl)
      mod_name 
      (mapcat " ;;\n\n" (sprint_decl (next lvl)) decls)
      (indent_string lvl) 
and sprint_decl lvl = function
  | DefVar (name,e) ->
     let w = sptf "let %s = " name in
     let z = get_indent_level w lvl in
     sptf "%s%s" w
       (sprint_exp z e)
  | DefFun (dfs) ->
     sprint_fun lvl dfs
  | DefFunRec (dfs) ->
     sprint_fun ~recflag:true lvl dfs
  | Sum (cstrs) -> "(* " ^ String.concat ", " cstrs ^ "*)"  (* TODO *)
and sprint_fun ?(recflag=false) lvl l =
  (indent_string lvl) ^
    (if recflag then "let rec" else "let") ^
      String.concat ("\n" ^ indent_string lvl ^ "and") @@
        List.map (fun (DF (name,args,e)) ->
            let s = sptf " %s %s = " name (String.concat " " args) in  
            s ^ "\n" ^ (indent_string (next lvl)) ^ (sprint_exp (next lvl) e)) l
and sprint_exp lvl = function
  | Constant(c) ->
     sprint_constant lvl c
  | Ident name -> name
  | Let(name,e1,e2) ->
     let w = sptf "(let %s = " name in
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
  | Fun(name,e) ->
     let w = sptf "(fun %s -> " name in
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
  | Block(es) ->
     "[|" ^ mapcat "; " (sprint_exp 0) es ^ "|]"
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
  | Match(e1,cases) ->
     let lvl = lvl + 1 in (* pour la parenthèse ouvrante *)
     sptf "(match %s with\n%s"
       (sprint_exp 0 e1)
       (String.concat "\n"
          (List.map 
             (function 
              | Case (c,e) ->
                 let s = sptf "%s| %s -> "
                           (indent_string lvl) (sprint_constant lvl c) in
                 let lvl' = get_indent_level s 0 in
                 s ^ (sprint_exp lvl' e)
              | Otherwise (e) ->
                 let s = sptf "%s| _ -> "
                           (indent_string lvl) in
                 let lvl' = get_indent_level s lvl in
                 s ^ (sprint_exp lvl' e))
             cases)) ^ ")"
  | Closure ((addr,e1),name,e2) -> sptf "#({%s}{%s})"
                                     (sprint_exp lvl e1)
                                     (sprint_exp lvl e2)
  (* ou bien :
     "(%s {scode(%d): %s | %s})" name addr (sprint_exp 0 e1) (sprint_exp 0 e2) 
  *)
  | Ext ext -> (match ext with
                | Array_alloc e ->
                   sptf "$alloc %s" (sprint_exp lvl e)
                | SetGlobal (e,n) ->
                   sptf "$set_global %s %d" (sprint_exp lvl e) n
                | ReadGlobal n ->
                   sptf "$read_global %d" n 
                | Goto (s,es) ->
                   sptf "$goto %s %s" s (mapcat " " (fun x -> sprint_exp lvl x) es)
                | Label (s,e) ->
                   sptf "$label %s;\n%s%s" s (indent_string lvl) (sprint_exp lvl e))
and sprint_constant lvl = function
  | Unit -> sptf "()"
  | Bool b -> sptf "%b" b
  | Int n -> sptf (if n >= 0 then "%d" else "(%d)") n
  | Char c -> sptf "%c" c
  | String s -> sptf "\"%s\"" s
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
