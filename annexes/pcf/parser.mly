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
  open Ast
%}

/* (* reserved words *) */
%token LET LETREC IFZ IN IF THEN ELSE INT FUN DOT ADD SUB EQ END LPAREN RPAREN EOF

%token <string> IDENT
%token <int> INT

%start prog
%type <Ast.exp>  prog
%nonassoc IN
%nonassoc LET
%nonassoc LETREC
%nonassoc FUN
%nonassoc DOT
%left ADD
%left SUB
%%

prog:
| e DOT { $1 }
| e EOF { failwith "Manque un point à la fin de la phrase" }
;

e:
| e ADD e          { Add($1,$3) }
| e SUB e          { Sub($1,$3) }
| app              { $1 }
;

app:
| exp        { $1 }
| app exp    { App($1,$2) }
;
exp:
| LET IDENT EQ e IN e       { Let($2,$4,$6) }
| LETREC IDENT EQ e IN e    { Letrec($2,$4,$6) }
| FUN IDENT DOT e           { Lam($2,$4) }
| LPAREN e RPAREN           { $2 }
| INT                       { Int($1)}
| IDENT                     { Var($1) }
| IFZ e THEN e ELSE e       { IfZero($2,$4,$6) } 
| error                     { failwith "erreur" }
;
