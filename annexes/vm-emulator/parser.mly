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
  open Bc
%}

%token EOF POP PUSH FUNCTION RETURN CALL GOTO IFGOTO LABEL DOT CONSTANT STATIC LOCAL ARGUMENT TEMP THIS THAT ADD SUB LAND LOR NOT LT GT EQ EXIT

%token <string> IDENT 
%token <string> FNAME 
%token <string> MIDENT
%token <int> INT

%start prog
%type <Bc.prog>  prog

%%

prog:
| EOF         { [] }
| inst prog   { $1::$2 }
;

inst:
| prim                   { Op($1) }
| POP segment            { Pop($2) }
| PUSH segment           { Push($2) }
| FUNCTION fun_name INT  { Function($2,$3) }
| RETURN                 { Return }
| CALL fun_name INT      { Call($2,$3) }
| GOTO label             { Goto($2) }   
| IFGOTO label           { IfGoto($2)  }   
| LABEL label            { Label($2) }  
;
prim:
| ADD     { Add }
| SUB     { Sub }
| LAND    { Land }
| LOR     { Lor }
| NOT     { Not }
| LT      { Lt }
| GT      { Gt }
| EQ      { Eq }
| EXIT    { Exit }
;

fun_name:
| FNAME { match String.split_on_char '.' $1 with 
           | [x;y] -> (x,y) }
;

label:
| IDENT     { $1 }
| MIDENT     { $1 }
;

segment:
| CONSTANT INT               { Constant($2) }
| STATIC INT                { Static($2) }
| LOCAL INT                 { Local($2) }
| ARGUMENT INT              { Argument($2) }
| THIS INT                  { This($2) }
| THAT INT                  { That($2) }
| TEMP INT                  { Temp($2) }
;
