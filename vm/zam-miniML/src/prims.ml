(**************************************************************************)
(*                                                                        *)
(*         PSTL : OCaml sur la plate-forme Nand2Tetris (2020)             *)
(*                                                                        *)      
(*           Loïc SYLVESTRE              Pablito BELLO                    *)
(*           loic.sylvestre@etu.upmc.fr  pablito.bello@etu.upmc.fr        *)
(*                                                                        *)  
(**************************************************************************)

let isint v = 
  if Mlvalues.is_ptr v then 0 else 1

(*** opérations arithmétiques ***)

let negint n = (-n)
let addint n1 n2 = n1 + n2 
let subint n1 n2 = n1 - n2
let mulint n1 n2 = (n1 * n2)

(*** division d'après la doc OCaml : (-x) / y = x / (-y) = -(x / y). ***)
(*** (/) 17 2       ~>  8  *)
(*** (/) 17 (-2)    ~> -8  *)
(*** (/) (-17) 2    ~> -8  *)
(*** (/) (-17) (-2) ~>  8  *)

let rec div_aux n1 n2 accu = 
  if n1 < n2 then accu
  else div_aux (n1 - n2) n2 (accu + 1)

let div n1 n2 = div_aux n1 n2 0

let divint n1 n2 =
  if n2 = 0 then failwith "divint" else
    if n1 >= 0 then (if n2 >= 0 then div n1 n2 else - (div n1 (- n2)))
    else (if n2 >= 0 then - (div (- n1) n2) else (div (- n1) (- n2)))

(*** modulo d'après la doc OCaml : `((mod) x y) < 0` si et seulement si `x < 0` ***)
(*** (mod) 11 3        ~>  2 *)
(*** (mod) 11 (-3)     ~>  2 *)
(*** (mod) (-11) 3     ~> -2 *)
(*** (mod) (-11) (-3)  ~> -2 *)

let rec modulo n1 n2 =
  if n1 < n2 then n1
  else modulo (n1 - n2) n2

let rec modint n1 n2 = (* dans un premier temps, on suppose que n2 >= 0 ***)
  if n2 = 0 then failwith "modint" else
    if n1 < 0 then - (modulo (- n1) (abs n2)) else (modulo n1 (abs n2))


(*** opérations logiques ***)

let andint n1 n2 = 
  if n1 <> 0 && n2 <> 0 then 1 else 0

let orint n1 n2 = 
  if n1 <> 0 || n2 <> 0 then 1 else 0

let xorint n1 n2 = 
  if (n1 <> 0 && n2 = 0) || (n1 <> 0 && n2 = 0) then 1 else 0

let bnot n = 
  if n = 0 then 1 else 0

(*** opérations de décalage ***)
(*** dans un premier temps, on suppose que le déplacement est toujours >= 0 ***)
(*** dans un premier temps, on ne fait pas de distinction entre lsr et asr  ***)
let rec lslint n dep =
  if dep = 0 then n
  else lslint (n + n) (dep - 1)

let rec lsrint n dep =
  if dep = 0 then n
  else lsrint (n / 2) (dep - 1)

let asrint n1 n2 = lsrint n1 n2 

(*** opérations de comparaison ***)

(*** égalité physique ***)
let eq n1 n2 = 
  if n1 = n2 then 1 else 0

(*** différence physique ***)
let neq n1 n2 = 
  if n1 <> n2 then 1 else 0

let ltint n1 n2 = 
  if n1 < n2 then 1 else 0

let leint n1 n2 = 
  if n1 <= n2 then 1 else 0

let gtint n1 n2 = 
  if n1 > n2 then 1 else 0

let geint n1 n2 = 
  if n1 >= n2 then 1 else 0

let compare_imm n1 n2 =
  if n1 < n2 then -1 else if n1 > n2 then 1 else 0

(*** comparaison (<) non signée               ***)
(*** n1 < 0 && n2 >= 0 => (ultint n1 n2) ~> 0 ***)
let ultint n1 n2 =
  if n1 < 0 then (if n2 < 0 then gtint n1 n2 else 0)
  else if n2 < 0 then 0 else ltint n1 n2

(*** comparaison (>=) non signée              ***)
(*** n1 < 0 && n2 >= 0 => (ugeint n1 n2) ~> 1 ***)
let ugeint n1 n2 =
  if n1 < 0 then (if n2 < 0 then leint n1 n2 else 1)  
  else if n2 < 0 then 1 else geint n1 n2

