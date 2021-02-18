(**************************************************************************)
(*                                                                        *)
(*         PSTL : OCaml sur la plate-forme Nand2Tetris (2020)             *)
(*                                                                        *)      
(*           Loïc SYLVESTRE              Pablito BELLO                    *)
(*           loic.sylvestre@etu.upmc.fr  pablito.bello@etu.upmc.fr        *)
(*                                                                        *)  
(**************************************************************************)


let prefix = "ML_"

let indent = "    "

let rec string_of_instrs f b =
  List.iter f (List.map (fun x -> string_of_instr x) b)    
and string_of_instr inst = 
  match inst with
  | Bc.Comment(s) -> (^) ((^) indent "// ") s
  | Bc.Push(pt) -> (^)  ((^) indent "push ") (string_of_segment pt)
  | Bc.Pop(pt) -> (^) ((^) indent ("pop ")) (string_of_segment pt)
  | Bc.Label(k) -> (^) "label " k
  | Bc.IfGoto(k) -> (^) indent ((^) "if-goto " k)
  | Bc.Goto(k) -> (^) indent ((^) "goto " k)
  | Bc.Return -> (^)  indent "return"
  | Bc.Function (f,n) ->
     (* sptf "// val \"%s\"\n\ " *)
       (^) "function " ((^) ((^) prefix f) ((^) " " (string_of_int n)))
  | Bc.Call (f,n) -> (^) indent ((^) "call "
                                   ((^) ((^) prefix f) ((^) " " (string_of_int n))))
  | Bc.BinOp(s) -> string_of_binop s
  | Bc.UnOp(s) -> string_of_unop s
  | Bc.True -> (^) ((^) indent "push constant 0\n") ((^) indent "not")
  | Bc.False -> ((^) indent "push constant 0\n")
and string_of_segment seg = 
match seg with
  | Bc.Anywhere ->
     string_of_segment (Bc.Temp 0)
  | Bc.Argument(n) ->
     (^) "argument " (string_of_int n)
  | Bc.Constant(n) ->
  if n >= 0 then (^)((^) indent "constant ") (string_of_int n)
  else (^) ((^) indent "constant 0\n    push constant ")
         ((^) (string_of_int (-n)) "\n    sub")
  | Bc.Static(n) ->
     (^) "static " (string_of_int n)
  | Bc.Local(n) ->
     (^) "local " (string_of_int n)
  | Bc.That(n) ->
     (^) "that " (string_of_int n)
  | Bc.Temp(n) ->
     (^) "temp " (string_of_int n)
  | Bc.Pointer(n) ->
     (^) "pointer " (string_of_int n)
and string_of_binop op = 
match op with
  | Bc.Add -> (^) indent "add"
  | Bc.Sub -> (^) indent "sub"
  | Bc.Eq -> (^) indent "eq"
  | Bc.Gt -> (^) indent "gt"
  | Bc.Lt -> (^) indent "lt"
  | Bc.And -> (^) indent "and"
  | Bc.Or -> (^) indent "or"
  | Bc.Mult -> "call Math.multiply 2"
  | Bc.Div -> "call Math.divide 2"
  | Bc.Assign -> "call Memory.poke 2"
and string_of_unop op =
match op with
  | Bc.Not -> (^) indent "not"
  | Bc.Access -> "call Memory.peek 1"
  | Bc.Alloc -> "call Memory.alloc 1"
  
