(**************************************************************************)
(*                                                                        *)
(*         PSTL : OCaml sur la plate-forme Nand2Tetris (2020)             *)
(*                                                                        *)      
(*           Loïc SYLVESTRE              Pablito BELLO                    *)
(*           loic.sylvestre@etu.upmc.fr  pablito.bello@etu.upmc.fr        *)
(*                                                                        *)  
(**************************************************************************)

open Kbc

let code = [||]

let pc = ref 0

let stack = Array.make 20 0
let sp = ref 0

(* that *)
let heap = Array.make 32000 0
let hp = ref 0


let temp = Array.make 8 0
let this = Array.make 2 0

let static = Hashtbl.create 128

type frame =  { mutable sp : int; 
                mutable nb_args : int; 
                mutable nb_locals : int; 
                mutable link : int }

let frame = { sp = 0;
              nb_args = 0; 
              nb_locals = 0; 
              link = (-1) }

let print_stack () =
  Array.mapi (Printf.printf "pile(%d) = %d\n") stack

let print_frame () =
  Printf.printf "sp = %d ; nb_args = %d ; nb_locals = %d; link = %d \n"
    frame.sp frame.nb_args frame.nb_locals frame.link

let pop_stack () =
  Pervasives.decr sp;
  let x = stack.(!sp) in
  stack.(!sp) <- (-1);
  x


let pop_n_stack n =
  sp := !sp - n 

let push_stack x =
  stack.(!sp) <- x;
  Pervasives.incr sp

let pop_segment s =
  let x = pop_stack () in
  match s with
  | Constant n -> assert false
  | Local n -> Printf.printf "~~~>%d\n"
                 (frame.sp - frame.nb_locals + n);
               stack.(frame.sp - frame.nb_locals + n) <- x
  | Argument n -> stack.(frame.sp - frame.nb_locals - frame.nb_args -1 + n) <- x
  | Temp n -> temp.(n) <- x
  | This n -> this.(n) <- x
  | That n -> heap.(n) <- x
  | Static (i,n) -> Hashtbl.replace static (i,n) x
let push_segment s = 
  push_stack (
      match s with
      | Constant n -> n
      | Local n -> stack.(frame.sp - frame.nb_locals + n)
      | Argument n -> stack.(frame.sp - frame.nb_locals - frame.nb_args -1 + n)
      | Temp n -> temp.(n) 
      | This n -> this.(n) 
      | That n -> heap.(n)
      | Static (i,n) -> Hashtbl.find static (i,n)
    )

let binop f = 
  let x2 = pop_stack () in
  let x1 = pop_stack () in
  Printf.printf "%d %d %d\n" x1 x2 (f x1 x2);
  push_stack (f x1 x2)

exception Exit
let rec interp start n code =
  pc := start;
  sp := n; 
  frame.sp <- !sp;
  frame.nb_args <- 0;
  frame.nb_locals <- n;
  frame.link <- -1;
  try 
    while true do
      print_frame ();
      print_stack ();
      Printf.printf "%d: %s\n" !pc (string_of_instr code.(!pc));  
      begin
        match code.(!pc) with
        | Pop s -> pop_segment s
        | Push s -> push_segment s
        | Call(l,locals,arity) -> print_stack ();
                                  

                                  push_stack frame.sp;
                                  push_stack frame.nb_args;
                                  push_stack frame.nb_locals;
                                  push_stack frame.link;
                                  sp := !sp + locals;
                                  frame.link <- !pc;
                                  frame.nb_locals <- locals;
                                  frame.nb_args <- arity;
                                  frame.sp <- !sp;
                                  pc := l - 1
        | Return -> sp := frame.sp;
                    pop_n_stack (frame.nb_args + frame.nb_locals - 1);
                    if !sp < 0 then raise Exit;
                    pc := frame.link;
                    frame.link <- pop_stack ();
                    frame.nb_locals <- pop_stack ();
                    frame.nb_args <- pop_stack ();
        | IfGoto n -> let x = pop_stack () in
                      if x <> 0 then pc := n - 1
        | Goto n -> pc := n - 1
        | Op (Add) -> binop (+)
        | Op (Sub) -> binop (-)
        | Op (Eq) -> binop (fun n1 n2 -> if n1 = n2 then 1 else 0)
        | Op (Lt) -> binop (fun n1 n2 -> if n1 < n2 then 1 else 0)
        | Op (Gt) -> binop (fun n1 n2 -> if n1 < n2 then 1 else 0)
        | Op (Not) -> let x1 = pop_stack () in
                      push_stack (if x1 = 0 then 0 else 1)
        | Op (Land) -> binop (land)
        | Op (Lor) -> binop (lor)
        | Prim(ML_exit) -> raise Exit
        | Prim(ML_array_length) -> let a = pop_stack () in
                                   push_stack heap.(a)
        | Prim(ML_array_get) -> let i = pop_stack () in
                                let a = pop_stack () in
                                push_stack heap.(a+i+1)
        | Prim(ML_array_set)  -> let x = pop_stack () in
                                 let i = pop_stack () in
                                 let a = pop_stack () in
                                 heap.(a+i+1) <- x;
                                 push_stack 0
        | Prim(ML_array_create_uninitialized) -> 
           let len = pop_stack () in
           let a = !hp in
           heap.(a) <- len;
           hp := len + 1;
           push_stack a
        | Prim(ML_array_make) -> 
           let init = pop_stack () in
           let len = pop_stack () in
           let a = !hp in
           heap.(a) <- len;
           hp := len + 1;
           for i = a + 1 to !hp - 1 do
             heap.(i) <- init 
           done;
           push_stack a 
        | Prim(ML_print_int) -> 
           (* Printf.printf "n = %d\n" (!sp - frame.nb_locals - frame.nb_args -4 + n);*) 

           let n = pop_stack () in print_int n; push_stack 0
        (* ;print_string "\n";exit 0*)
        | Prim(ML_print_char) ->
           let n = pop_stack () in
           print_char (Char.chr n);
           push_stack 0
        | Prim(ML_print_newline) -> let _ =  pop_stack () in
                                    print_newline ();
                                    push_stack 0
        | Prim(ML_print_char_array) -> let a = pop_stack () in
                                       let len = heap.(a) in
                                       for i = a + 1 to a + len do
                                         print_char (Char.chr heap.(i))
                                       done;
                                       push_stack 0
      (* | ML_make_pair -> "ML_make_pair" 
    | ML_left -> "ML_left"
    | ML_right -> "ML_right"
    | ML_obj_magic -> "ML_obj_magic" *)

      end;
      Pervasives.incr pc
    done
  with Exit -> print_newline ()
