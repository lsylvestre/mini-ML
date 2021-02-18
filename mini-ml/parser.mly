/**************************************************************************/
/*                                                                        */
/*         PSTL : OCaml sur la plate-forme Nand2Tetris (2020)             */
/*                                                                        */      
/*           Loïc SYLVESTRE              Pablito BELLO                    */
/*           loic.sylvestre@etu.upmc.fr  pablito.bello@etu.upmc.fr        */
/*                                                                        */  
/**************************************************************************/

%{
  open Parseutils
  open Past

let decl_create d = Past.{decl_desc = d; decl_loc = pos()}
let exp_create e = Past.{exp_desc = e; exp_loc = pos()}


%}

/* (* reserved words *) */
%token LET WHERE IN IF THEN ELSE ASSERT WHILE FOR TO DO DONE MATCH WITH PIPE BEGIN END EXTERNAL AND_KW CONS
%token UNIT_TY BOOL_TY INT_TY STRING_TY ARRAY_TY ATAT FUN SHARP OF IMPLY CAT AT REF

%token <string> IDENT IDENT_CAPITALIZE VM_IDENT
%token <string> STRING
%token <char> CHAR
%token <int> INT
%token <bool> BOOL

%token <string> TVAR

%token PLUS MINUS TIMES DIV AND OR LAND LOR EQ NEQ GT LT GE LE TRUE FALSE TYPE
%token REC
/* (* control characters *) */
%token EOF TERMINAISON DOT COLON LPAREN RPAREN LBRACKET RBRACKET SEMICOL BEGIN END COMMA OF
%token ARRAY_OPEN ARRAY_CLOSE ARRAY_ACCESS_OPEN LEFT_ARROW RIGHT_ARROW ASSIGN ACCESS WILDCARD

%nonassoc LET 
%nonassoc WHERE 
%right SEMICOL
%nonassoc IN
%nonassoc ARRAY_OPEN ARRAY_CLOSE

/* ATAT */

%right COLON /* lowest precedence */ 
%nonassoc  IF
%right     LEFT_ARROW ASSIGN
/* %right     COMMA */
%right     CONS /* ??? */
%left      OR
%left      AND
%left      EQ NEQ GT GE LT LE
%left      PLUS MINUS     
%left      LAND
%left      LOR 
%left TIMES DIV     CAT AT /* ? */              
%left      DOT  
%left      ACCESS                
%nonassoc  IDENT LPAREN RPAREN BEGIN END        /* highest precedence */        

%start tmodule         /* the entry point */

%type <Past.decl list>  tmodule
%type <Past.exp>        expr
%type <Past.typ>       exp_ty
%type <Past.match_case> match_case

%%

tmodule:
decls {$1}
;

decls :
 | EOF                      { [] }
 | decl decls               { $1 @ $2 }
 | decl terminaison decls   { $1 @ $3 }
 | { error_exit (pos()) "programme malformé" }
 ;

 terminaison:
 |                         {}
 | TERMINAISON terminaison {}
 | error { error_exit (pos()) "fin de phrase. `;;` attendues." }
 ;

decl : 
 | seq terminaison                            { [decl_create @@ DefVar(("_",None),$1)] }
 | LET argument EQ seq                        { [decl_create @@ DefVar($2,$4)] }
 | LET defuns                                 { [decl_create @@ DefFun($2)] }
 | LET REC defuns                             { [decl_create @@ DefFunRec($3)] }
 | decl_types                                 { List.rev $1 }
 | LET error { error_exit (pos()) "déclaration `let` malformée. J'attend {let <ident> [...] = <expr> in <expr>}" }
 | error { error_exit (pos()) "déclaration malformée (`let` ou `type` attendu)" }
 ;

decl_type:
 | param_type_decl IDENT EQ ty { decl_create @@ Type($2,$1,$4) }
 | error { error_exit (pos()) "..." }
 ;

 decl_types:
 | TYPE decl_types_aux { $2 }
 ;
 decl_types_aux:
 | decl_type { [$1] }
 | decl_type AND_KW decl_types_aux { $1::$3 }
 ;

defun:
| ident arguments EQ seq { ($1,$2,None,$4) }
| ident arguments COLON exp_ty EQ seq { ($1,$2,Some $4,$6) }
;

ident: 
| IDENT {$1}
| CAT { "(^)"}

defuns:
| defun                {[$1]}
| defun AND_KW defuns  {$1::$3}
;
ignore:
| WILDCARD {}
| LPAREN RPAREN {}
;

ty :
 | exp_ty         { Exp_ty($1) }
 | PIPE sum_ty    { Sum($2) }
 | sum_ty         { Sum($1) }
 ;

sum_ty:
| sum_ty_cc               { [$1] }
| sum_ty_cc PIPE sum_ty   { $1::$3 }
;


sum_ty_cc:
| constructor                      { ($1,[]) } 
| constructor OF ty_cst_parameters { ($1,$3) } 
;

ty_cst_parameters:
| exp_ty_cstrparam                       { [$1] }
| LPAREN ty_cstp_aux RPAREN              { $2 }
| ty_cstp_aux { error_exit (pos()) "bien penser à parenthéser les motif ( . * . * . * .)"  }
;
ty_cstp_aux:
| exp_ty_cstrparam                    { [$1] }
| exp_ty_cstrparam TIMES ty_cstp_aux  { $1::$3 }
;
constructor :
| LBRACKET RBRACKET                     { "[]" }
| LPAREN CONS RPAREN                    { "::" }
| IDENT_CAPITALIZE                      { $1 }
| IDENT_CAPITALIZE DOT constructor      { $1 ^ "." ^ $3}
;


exp_ty_cstrparam:
 | LPAREN exp_ty RPAREN         { $2 }
 | ident_ty                     { $1 }
 | TVAR                         { Tvar $1 }
 | error { error_exit (pos()) "expression de type malformée." }
;

exp_ty:
 | LPAREN exp_ty RPAREN         { $2 }
 | ident_ty                     { $1 }
 | TVAR                         { Tvar $1 }
 | exp_ty TIMES exp_ty          { Tproduct($1,$3) }
 | exp_ty RIGHT_ARROW exp_ty    { Tarrow($1,$3) }
 | error { error_exit (pos()) "expression de type malformée." }
;

ident_ty:
| ident_in_mod                  { Tident($1) }
| IDENT                         { match $1 with 
                                 | "int" -> Tint
                                 | "unit" -> Tunit
                                 | "bool" -> Tbool
                                 | "char" -> Tchar
                                 | "string" -> Tstring
                                 | s -> Tident(s) }
| exp_ty IDENT                 { match $2 with 
                                   | "array" -> Tarray $1 
                                   | "ref" -> Tref $1
                                   | s -> Tconstr(s,[$1])  }
;


param_type_decl:
|                                       { [] }
| LPAREN param_type_decl_aux RPAREN     {$2}
| TVAR                                 {[$1]}
|  error { error_exit (pos()) "(('a,'b ...) t)" }
;
param_type_decl_aux:
| TVAR                               { [$1] }
| TVAR COMMA param_type_decl_aux      { $1::$3 }


ident_in_mod:
| IDENT                  { $1 }
| IDENT_CAPITALIZE DOT ident_in_mod { $1 ^ "." ^ $3 }
;

seq :
| expression                 { $1 }
| expression SEMICOL seq     { exp_create @@ Seq($1,$3) }
;

expression : 
| expr                                   { $1 }
| FUN argument RIGHT_ARROW seq        { exp_create @@ Fun($2,$4) }
| LET argument EQ seq IN seq          { exp_create @@ Let($2,$4,$6) }
| LET defuns IN seq                      
 { 
    List.fold_right
       (fun (name,args,tyopt,e) exp ->
      exp_create @@ Let((name,None),
        List.fold_right 
          (fun a e -> exp_create @@ Fun(a,e)) 
          args (match tyopt with
            | None -> e 
            | Some ty -> exp_create @@ Annotation(e,ty)),
        exp))
         $2 $4}
| LET REC ident EQ seq IN seq                      
 { exp_create @@ LetRec($3,$5,$7) }
/* | LET REC error { error_exit (pos()) "pas de construction let rec local" } */
| expression WHERE argument EQ seq   { exp_create @@ 
                                         match $3 with 
                                         | "_",None -> Seq($5,$1)
                                         | "_",Some t -> Seq(exp_create @@ Annotation($5,t),$1)
                                         | x,tyopt -> Let((x,tyopt),$5,$1) }
| IF seq THEN expression ELSE expression { exp_create @@ If($2,$4,$6) }
| IF seq THEN expression                 { exp_create @@ If($2,$4,exp_create @@ Constant(Unit))}
| MATCH seq WITH match_body              { exp_create @@ Match($2,$4)}
| WHILE seq DO seq DONE                  { exp_create @@ While($2,$4) }
| FOR IDENT EQ seq TO seq DO seq DONE    { exp_create @@ For($2,$4,$6,$8) }
;

argument:
| LPAREN argument RPAREN                     { $2 }
| LPAREN RPAREN                              { ("_",Some Tunit) } 
| argument_aux                               { ($1,None)}
| argument_aux COLON exp_ty                 { ($1,Some $3) }
| error { error_exit (pos()) "argument malformé." }
;
argument_aux:
| IDENT                                         { $1 }
| WILDCARD                                      { "_" } 
;


argu_p:
| IDENT                                { ($1,None) }
| LPAREN IDENT COLON exp_ty RPAREN    { ($2,Some $4) } 
| WILDCARD                             { ("_",None) } 
| LPAREN RPAREN                        { ("_",Some Tunit) } 
| LPAREN argu_p RPAREN          { $2 } 
| error { error_exit (pos()) "argument malformé." }
;

arguments : 
| argu_p                 { [$1] }
| argu_p arguments       { $1::$2 }
| error { error_exit (pos()) "liste d'arguments malformée." }
;

expr: 
 | app                                   { $1 } 
 | expression PLUS expression            { exp_create @@ BinOp(Add,$1,$3) }
 | expression MINUS expression           { exp_create @@ BinOp(Minus,$1,$3) }
 | expression TIMES expression           { exp_create @@ BinOp(Mult,$1,$3) }
 | expression DIV expression             { exp_create @@ BinOp(Div,$1,$3) }
 | expression EQ expression              { exp_create @@ BinOp(Eq,$1,$3) }
 | expression NEQ expression             { exp_create @@ BinOp(Neq,$1,$3) }
 | expression GT expression              { exp_create @@ BinOp(Gt,$1,$3) }
 | expression LT expression              { exp_create @@ BinOp(Lt,$1,$3) }
 | expression GE expression              { exp_create @@ BinOp(Ge,$1,$3) }
 | expression LE expression              { exp_create @@ BinOp(Le,$1,$3) }
 | expression OR expression              { exp_create @@ BinOp(Or,$1,$3) }
 | expression AND expression             { exp_create @@ BinOp(And,$1,$3) }
 | expression LOR expression             { exp_create @@ BinOp(Lor,$1,$3) }
 | expression LAND expression            { exp_create @@ BinOp(Land,$1,$3) }
 | expr ASSIGN expression                { exp_create @@ Ref_assign($1,$3) } 
 | REF exp                               { exp_create @@ Ref($2) }
 | MINUS expr                            { exp_create @@ UnOp(UMinus,$2) }
 | expression COMMA expression           { exp_create @@ Pair($1,$3) }
 | expression CONS expression            { exp_create @@ App(exp_create @@ Constant(Constr("::")),[$1;$3]) } 
 | expression AT expression              { exp_create @@ App(exp_create @@ Ident("List.append"),[$1;$3]) }
 
/* | error                                 { raise (Parse_Exception ("malformed expression ")) }*/
;

app:
 | exp                                   { $1 }
 | exp exprs                             { exp_create @@ App($1,$2) }
 | exp ATAT app                          { exp_create @@ App($1,[$3]) }
 | SHARP exp                             { exp_create @@ Magic($2) }
 | ASSERT exp                            { exp_create @@ Assert($2,pos()) }
 | extra_app_constructor                 { $1 }
 ;

exprs :
 | exp        { [$1] }
 | exp exprs  { $1::$2 }
 ;

exp:
| ACCESS exp                            { exp_create @@ Ref_access($2) } 
| LPAREN expression COLON exp_ty RPAREN { exp_create @@ Annotation($2,$4) }
| LPAREN seq RPAREN                     { $2 }
| BEGIN seq END                         { $2 }
| constant                              { exp_create @@ Constant($1) }
| ident                                 { exp_create @@ Ident($1) }
| ident_in_mod                          { exp_create @@ Ident($1) }
| ARRAY_OPEN array_content ARRAY_CLOSE  { exp_create @@ Array_create($2) }
| exp ARRAY_ACCESS_OPEN seq RPAREN     { exp_create @@ Array_access($1,$3) }
| exp ARRAY_ACCESS_OPEN seq RPAREN LEFT_ARROW expression { exp_create @@ Array_assign($1,$3,$6) }
| error { error_exit (pos()) "expression malformée." }
;

constant:
 | LPAREN RPAREN                         { Unit }
 | INT                                   { Int($1) }
 | CHAR                                  { Char($1) }
 | BOOL                                  { Bool($1) }
 | STRING                                { String($1) }
 | constructor                           { Constr($1) }
 | ARRAY_OPEN ARRAY_CLOSE                { Array_empty }
 ;

match_body:
| match_body_aux       {$1}
| PIPE match_body_aux  {$2}

match_body_aux:
| match_case                     { [$1] }
| match_case PIPE match_body_aux { $1::$3 }
;
match_case:
| WILDCARD RIGHT_ARROW seq  { Otherwise($3) }
| app_cst RIGHT_ARROW seq  { let c,args = $1 in Case(c,args,$3) }
| error { error_exit (pos()) "match clause malformée." }
;

app_cst:
| constant cst_parameters    { ($1,$2) }
| constant cst_args          { ($1,$2) }
| argument_aux CONS argument_aux  { (Constr("::"),[$1;$3]) }
;

cst_parameters:
|                                  { [] }
| LPAREN cst_parameters_aux RPAREN { $2 }
;

cst_parameters_aux:
| argument_aux                          { [$1] }
| argument_aux COMMA cst_parameters_aux {$1::$3}
;


cst_args:
|              { [] }
| cst_args_aux { $1 }
;

cst_args_aux:
| argument_aux              { [$1] }
| argument_aux cst_args_aux {$1::$2}
;


array_content:
|                            { [] }
| array_content_aux          { $1 }
;

array_content_aux:
|                                       { [] }
| expression                            { [$1] }
| expression SEMICOL array_content_aux  { $1::$3 }
;


extra_app_constructor:
/* | constructor              { exp_create @@ Constant(Constr($1)) } */
| constructor tuple        { exp_create @@ App(exp_create @@ Constant(Constr($1)),$2) }
;

tuple:
/* | exp                      { [$1] } */
| LPAREN tuple_aux RPAREN  { $2 }
;

tuple_aux:
| expr                      { [$1] }
| expr COMMA tuple_aux      { $1::$3 }
;
