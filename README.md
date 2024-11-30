# mini-ML 

Projet de Master 1 STL à Sorbonne Université

## Auteurs

Pablito Bello et Loïc Sylvestre

sous la direction d'Emmanuel Chailloux

## Description

Implémentation de la machine virtuelle OCaml sur la chaîne de compilation pédagogique [Nand2Tetris](https://www.nand2tetris.org)  ; pour y parvenir, nous avons implanté un compilateur pour mini-ML (un sous-ensemble fonctionnel-impératif d’OCaml) en partie auto-amorçé (bootstrap) et produisant du bytecode exécutable par la machine virtuelle Nand2Tetris ; puis nous avons implémenté en Mini-ML la machine virtuelle OCaml et sa bibliothèque d’exécution, y compris un gestionnaire automatique de mémoire pour OCaml (GC Stop&Copy).

(voir rapport.pdf)

## Installation

Opam doit être installé. Voici les dépendances :

```bash
$ opam switch create ocaml-base-compiler.4.07.1
$ opam install dune
$ opam install obytelib
$ opam install ocamlclean
```

La version de dune doit être dune 1.11, vous pouvez-aussi essayer de modifier le fichier dune-project avec votre version.

- NB: obytelib 1.5 nécessite ocaml >= 4.07, mais manipule du bytecode ocaml <= 4.07.1

### Compilation de la ZAM

Dans tous les exemples, `nom_du_fichier.ml` est à remplacer par le nom d'un fichier source OCaml.

- `make zam-miniML MLFILES=nom_du_fichier.ml` compile en `miniML` notre implémentation de la ZAM. Les exécutables sont dans 'zam/bin', accompagné d'un script 'Main.tst' permettant de configurer le simulateur Nand2Tetris. Pour lancer le programme depuis le simulateur, on ouvrira Main.tst avec *file -> load script*, on pourra enlever les animations pour rendre la VM plus rapide : *animate -> no animation*, puis on cliquera sur run (bouton double flèche bleue).

- `make zam-ocaml MLFILES=nom_du_fichier.ml` compile en OCaml notre implémentation de la ZAM. L'exécutable est 'zam/src/ocaml/zam.exe'

- `make zam-miniML-run MLFILES=nom_du_fichier.ml` compile en mini-ml notre implémentation de la ZAM, puis lance le simulateur

- `make zam-ocaml-run MLFILES=nom_du_fichier.ml` compile en OCaml notre implémentation de la ZAM, puis lance une implémetation Java de la VM `Nand2Tetris` : *VMemulator*.

Pour lancer un programme séparé en plusieurs fichiers, toutes les commandes précédentes peuvent contenir plusieurs noms de fichiers.

Exemple :

```bash
$ make zam-ocaml-run MLFILES="nom_du_fichier1.ml nom_du_fichier2.ml"
```

Exemple concret d'utilisation :

```bash
$ make zam-miniML-run MLFILES=benchs/fact.ml
```

### Tests unitaires 

Il est possible de tester notre implémentation de la ZAM, compilé par `ocamlc`, sur un ensemble de fichiers de tests. Pour cela, il suffit de saisir : 

- `make test`



### Options de compilation de notre implémentation de la ZAM en `miniML`.

Les options du compilateur `miniML`peuvent être ajouté en redéfinissant, dans le Makefile prinicipal, la constante   `MINIML-FLAGS`.

Exemple : 

```bash
make zam-miniML-run MLFILES="benchs/fact.ml" MINIML-FLAGS="-printast"
```

Les options disponibles sont :

```
- -printpast  : affiche l'AST en syntaxe Caml
- -printast  : affiche l'AST simplifié en syntaxe Caml (après typage et optimisation)
- -typecheck  : type le programme est abandonne si celui-ci est mal typé
- -inline  : profondeur d'inlining
- -noglobalize  : désactive la globalisation des valeurs immutables allouées.
- -nofolding  : désactive la propagation des constantes
- -nosmpvar  : désactive la réécriture des variables globales de la forme [let x = constante] en fonction d'arité 0
- -src  : spécifie où chercher les fichiers sources à compiler
- -dst  : spécifie le dossier où seront placés les fichiers compilés
- -stdlib chemin vers la bibliothèque d'exécution de mini-ml
- -assert  : embarque les assertions dans le code
```

### Etapes intermédiaires

- `make miniML` compile (en OCaml) le compilateur `mini-ml`.
- `make link MLFILES="f1.ml f2.ml ..."` compile les sources avec ocamlc, avec un support à l'édition de lien en cas d'appels de fonctions extérieures.
- `make ocamlclean` produit l'exécutable `vm/link/byte.out` à partir de l'exécutable `vm/link/a.out`
- `make obytelib` construit le tableau code `zam/input.ml` à partir de l'exécutable `vm/link/byte.out`. 

### Organisation des sources
```text
.
├── Makefile
├── vm
│   ├── zam-miniML       # implantation de la ZAM en miniML
│   │   ├── bin
|   |   |   ├── Main.tst ;; exécutable
│   │   │   └── *.vm  
│   │   ├── input.ml
│   │   └── src
│   │       ├── mlvalues.ml  
│   │       ├── domain.ml 
│   │       ├── block.ml 
│   │       ├── data.ml   
│   │       ├── alloc.ml    
│   │       ├── gc.ml  
│   │       ├── prims.ml  
│   │       ├── call.ml  
│   │       ├── interp.ml  
│   │       └── main.ml  
│   ├── zam-OCaml         # implantation de la ZAM en OCaml
│   │   ├── zam.exe      ;; exécutable
│   │   ├── mlvalues.ml
│   │   └── Makefile     ;; le code source est identique à zam-miniML, excepté mlvalues.ml
│   ├── stdlib
│   │   ├── pervasives.ml
│   │   ├── array.ml
│   │   └── list.ml
│   ├── bytecode          # préparation du bytecode
│   │   └── obytelibParser.ml
│   └── link              # production d'un exécutable nettoyé par ocamlclean
│       ├── primitives.c
│       └── Makefile
├── mini-ml              # implantation en OCaml d'un compilateur (miniML -> Nand2Tetris)
│   ├── README.md
│   ├── generated_files
│   │   └── *.vm 
│   ├── stdlib
│   │   ├── array.ml
│   │   ├── list.ml
│   │   ├── Main.vm
│   │   ├── ML_Internal.vm
│   │   ├── pervasives.ml
│   │   └── string.ml
│   ├── Makefile
│   ├── parseutils.ml
│   ├── lexer.mll
│   ├── lexer.ml
│   ├── parser.mli
│   ├── parser.ml
│   ├── parser.mly
│   ├── past.ml
│   ├── ast.ml
│   ├── kast.ml
│   ├── bc.ml
│   ├── past_print.ml
│   ├── ast_print.ml
│   ├── bc_print.ml
│   ├── past2ast.ml
│   ├── ast2kast.ml
│   ├── kast2bc.ml
│   ├── types.ml
│   ├── typing.ml
│   ├── runtime.ml
│   ├── main.ml
│   ├── gensym.ml
│   ├── freevr.ml
│   ├── bc_fold.ml
│   ├── ast_tailrec.ml
│   ├── ast_smpvar.ml
│   ├── ast_inline.ml
│   ├── ast_lift.ml
│   ├── ast_fold.ml
│   ├── ast_globz.ml
│   └── ast_closure.ml
└── annexes
    ├── pcf         # implantation en OCaml d'un compilateur ([λ-calcul + fix] -> Nand2Tetris)
    └── vm-emulator # implantation en OCaml d'un simulateur Nand2Tetris
```
