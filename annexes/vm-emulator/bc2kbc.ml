(**************************************************************************)
(*                                                                        *)
(*         PSTL : OCaml sur la plate-forme Nand2Tetris (2020)             *)
(*                                                                        *)      
(*           Loïc SYLVESTRE              Pablito BELLO                    *)
(*           loic.sylvestre@etu.upmc.fr  pablito.bello@etu.upmc.fr        *)
(*                                                                        *)  
(**************************************************************************)


let main_main_pos = ref None

let kop = function
  | Bc.Add -> Kbc.Add
  | Bc.Sub -> Kbc.Sub
  | Bc.Eq -> Kbc.Eq
  | Bc.Not -> Kbc.Not
  | Bc.Lt -> Kbc.Lt
  | Bc.Gt -> Kbc.Gt
  | Bc.Land -> Kbc.Land
  | Bc.Lor -> Kbc.Lor
  | Bc.Exit -> Kbc.Exit


let segment i = function
  | Bc.Static (n) -> Kbc.Static (i,n)
  | Bc.Temp (n) -> Kbc.Temp (n) 
  | Bc.Argument (n) -> Kbc.Argument (n) 
  | Bc.Constant (n) -> Kbc.Constant (n)
  | Bc.Local (n) -> Kbc.Local (n)
  | Bc.This (n) -> Kbc.This (n) 
  | Bc.That (n) -> Kbc.That (n)

let instr i pc labels functions = function
  | Bc.Push seg -> Some (Kbc.Push (segment i seg))
  | Bc.Pop seg -> Some (Kbc.Pop (segment i seg))
  | Bc.Label l -> None
  | Bc.Goto l -> Some (Kbc.Goto (match List.assoc_opt l labels with 
                                 | None -> assert false
                                 | Some x -> x))
  | Bc.IfGoto l -> Some (Kbc.IfGoto (match List.assoc_opt l labels with 
                                     | None -> assert false
                                     | Some x -> x))
  | Bc.Function ((m,f),n) ->
     if (m,f) = ("Main","main") then main_main_pos := Some (pc,n);
     None
  | Bc.Return -> Some (Kbc.Return)
  | Bc.Call(("ML_Internal","exit"),_) -> Some (Kbc.Prim (ML_exit))
  | Bc.Call(("ML_Internal","array_length"),_) -> Some (Kbc.Prim (ML_array_length))
  | Bc.Call(("ML_Internal","array_get"),_) -> Some (Kbc.Prim (ML_array_get))
  | Bc.Call(("ML_Internal","array_set"),_) -> Some (Kbc.Prim (ML_array_set))
  | Bc.Call(("ML_Internal","array_create_uninitialized"),_) ->
     Some (Kbc.Prim (ML_array_create_uninitialized))
  | Bc.Call(("ML_Internal","array_make"),_) -> Some (Kbc.Prim (ML_array_make))
  | Bc.Call(("ML_Internal","print_int"),_) -> Some (Kbc.Prim (ML_print_int))
  | Bc.Call(("ML_Internal","print_char"),_) -> Some (Kbc.Prim (ML_print_char))
  | Bc.Call(("ML_Internal","print_newline"),_) -> Some (Kbc.Prim (ML_print_newline))
  | Bc.Call(("ML_Internal","print_char_array"),_) ->
     Some (Kbc.Prim (ML_print_char_array))
  | Bc.Call(("ML_Internal","make_pair"),_) -> Some (Kbc.Prim (ML_make_pair))
  | Bc.Call(("ML_Internal","left"),_) -> Some (Kbc.Prim (ML_left))
  | Bc.Call(("ML_Internal","right"),_) -> Some (Kbc.Prim (ML_right))
  | Bc.Call(("ML_Internal","obj_magic"),_) -> Some (Kbc.Prim (ML_obj_magic))
  | Bc.Call(mf,arity) ->
     let pc,nblocals =
       match List.assoc_opt mf functions with
       | None -> Printf.printf "-->%s.%s\n" (fst mf) (snd mf) ;assert false
       | Some x -> x in
     Some (Kbc.Call (pc,nblocals,arity))
  | Bc.Op (op) -> Some (Kbc.Op (kop op))

let prog l labels functions =
  let pc = ref 0 in
  let rec aux i acc = function
    | [] -> List.rev acc
    | ins::r -> (match instr i !pc labels functions ins with 
                 | None -> aux i acc r
                 | Some kbc -> incr pc; aux i (kbc :: acc) r) in
  List.mapi (fun i m -> aux i [] m) l
    
