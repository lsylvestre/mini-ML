# mini-ml

## structure du compilateur :
```
{input} -> (lexer) -> (parser) -> [PAST].
[PAST] -> (past_print) -> {OCaml}.
[PAST] -> (typing).                     (typage à la ML)
[PAST] -> (past2ast) -> [AST].
[AST] -> (ast_lift) -> [AST].           (lambda lifting)
[AST] -> (ast_inline) -> [AST].         (intégration d'appels de fonctions)
[AST] -> (ast_fold) ->  [AST].          (propagation de constantes)
[AST] -> (ast_globz) -> [AST].          (globalisation des valeurs allouées immutables)
[AST] -> (ast_print) -> {+/- OCaml}.
[AST] -> (ast_closure) -> [AST].        (introduction des fermetures)
[AST] -> (ast_tailrec) -> [AST].        (élimination des appels terminaux)
[AST] -> (ast2kast) -> [KAST].
[KAST] -> (kast2bc) -> [BC].
[BC] -> (bc_fold) -> [BC].              (propagation de constantes)
[BC] -> (bc_print) -> {VM nand2Tetris}.
```

## usage :
```
$ cd mini-ml
$ make
$ ./compile m1.ml m2.ml      # les fichiers générés sont dans "generated_files/"
$ VMEmulator.sh
~> click File->Load Script->exemple/bin/Main.tst
{click "animate" : No Animation}
{click "view" : Screen}
~> click Run->Run-> (Yes)
```

```
<prog> := <decl> <prog> 
        | <decl> ;; <prog>

<decl> := type ('a,...) <ident> = <exp_ty>
        | type ('a,...) <ident> = <constr> of (<exp_ty> * ... * <exp_ty>)
                                  # au plus 256 constructeurs par type
        | let <var> = <expr> 
        | let <ident> <arg> ... = <expr> 
          and <ident> <arg> ... = <expr> 
          ...
        | let rec <ident> <arg> ... = <expr> 
          and <ident> <arg> ... = <expr> 
          ...

<expr> := (<expr>)
        | (<expr> : <expr_ty>)
        | <constant>
        | (<expr>,<expr>)
        | <expr> :: <expr>
        | <constr> (<ident>,...,<ident>)
        | <expr> <binop> <expr>
        | <unop> <expr>
        | (fun <var> -> <expr>)
        | <ident> <expr> ...    # appel de fonction globale
        | <expr> <expr> ...     # appel de fonction calculée
        | let <ident> = <expr> in <expr>
        | let <expr> where <ident> = <expr>
        | let <ident> <arg> ... = <expr>      
          and <ident> <arg> ... = <expr> 
          ...
          in <expr> # fonction local
        | <expr> ; <expr>
        | if <expr> then <expr> else <expr>
        |   match <expr> with 
            | <constr> (<ident>,...,<ident>) -> <expr> 
            | ...
        | while <expr> do <expr> done
        | for <ident> = <expr> to <expr> do <expr> done
        | ref <expr>
        | !<expr>
        | <expr> := <expr>
        | [|<expr>;<expr>;...|]
        | <expr>.(<expr>)
        | <expr>.(<expr>) <- <expr>
        | assert <expr>   

<var> := <ident>
| (<ident> : <exp_ty>)
| _
| ()

<arg> := <ident> 
| (<ident> : <exp_ty>)
| _
| ()

<constant> := <bool>
            | <int>
            | ()
            | [||]
            | []
            | <constr>
            | <string>

<expr_ty> := (<expr_ty>,...) <ident>
           | <var_ty>
           | <expr_ty> * ... * <expr_ty>
           | <expr_ty> -> <expr_ty>

<binop> := + | - | = | < | <= | > | >= | <> | '&&' | '||'
<unop> := - | not


```
