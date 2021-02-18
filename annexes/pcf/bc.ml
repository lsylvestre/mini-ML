(**************************************************************************)
(*                                                                        *)
(*         PSTL : OCaml sur la plate-forme Nand2Tetris (2020)             *)
(*                                                                        *)      
(*           Loïc SYLVESTRE              Pablito BELLO                    *)
(*           loic.sylvestre@etu.upmc.fr  pablito.bello@etu.upmc.fr        *)
(*                                                                        *)  
(**************************************************************************)


type instrs = instr list          
and instr =
  | Push of (segment)
  | Pop of (segment)
  | Label of (int)
  | IfGoto of (int)
  | Goto of (int)
  | Function of (string * int) (* nb locales *)
  | Return
  | Call of (string * int) (* arité *)
  | Op of (op)
and segment =
  | Argument of (int)
  | Constant of (int)
  | Local of (int)
and op = Add | Sub | Eq | Not

let indent = "  "

let string_of_segment seg = 
  match seg with
  | Argument(n) ->
     (^) "argument " (string_of_int n)
  | Constant(n) ->
     if n >= 0 then (^) "constant " (string_of_int n)
     else (^) ((^) indent "constant 0\n    push constant ")
            ((^) (string_of_int (-n)) "\n    sub")
  | Local(n) ->
     (^) "local " (string_of_int n)
let string_of_op op = 
  match op with
  | Add -> (^) indent "add"
  | Sub -> (^) indent "sub"
  | Eq -> (^) indent "eq"
  | Not -> (^) indent "not"
let string_of_instr inst = 
  match inst with
  | Push(pt) -> (^)  ((^) indent "push ") (string_of_segment pt)
  | Pop(pt) -> (^) ((^) indent ("pop ")) (string_of_segment pt)
  | Label(k) -> ((^) "label L" (string_of_int k))
  | IfGoto(k) -> (^) indent ((^) "if-goto L" (string_of_int k))
  | Goto(k) -> (^) indent ((^) "goto L" (string_of_int k))
  | Return -> (^)  indent "return"
  | Function (f,n) -> (^) "function " ((^) f ((^) " " (string_of_int n)))
  | Call (f,n) -> (^) indent ((^) "call " ((^) f ((^) " " (string_of_int n))))
  | Op(s) -> string_of_op s

let string_of_instrs b =
  (List.map (fun x -> string_of_instr x) b)  


