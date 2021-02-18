(**************************************************************************)
(*                                                                        *)
(*         PSTL : OCaml sur la plate-forme Nand2Tetris (2020)             *)
(*                                                                        *)      
(*           Loïc SYLVESTRE              Pablito BELLO                    *)
(*           loic.sylvestre@etu.upmc.fr  pablito.bello@etu.upmc.fr        *)
(*                                                                        *)  
(**************************************************************************)


let debug = false
let debug_opcode = false
let debug_pc = false
let debug_data = false

let pc = ref 0
let extra_args = ref 0 
let trap_sp = ref 0

let pop_stack () =
  assert (!Domain.sp > 0);
  let v = Domain.stack.(!Domain.sp - 1) in 
  decr Domain.sp;
  v

let pop_stack_ignore n =
  assert (!Domain.sp >= n);
  Domain.sp := !Domain.sp - n

let stack_overflow () = 
  failwith "stack overflow"

let push_stack v =
  if !Domain.sp >= Domain.stack_size - 1 then stack_overflow (); 
  Domain.stack.(!Domain.sp) <- v; incr Domain.sp

let take_argument code =
  incr pc;
  code.(!pc)

let rec debug_print_block block =
  begin
    print_string "(block[";
    print_int (Mlvalues.ptr_val block);
    print_string "], size : ";
    print_int (Block.size (Mlvalues.ptr_val block));
    print_string ", tag : ";
    print_int (Block.tag (Mlvalues.ptr_val block));
    print_string ") ";
    print_string "{";
    (* attention, cela peut boucler sur des valeurs cycliques : *)
    (*
       for i = 0 to Block.size (Mlvalues.ptr_val block) - 1 do 
        print_string "<";
        if Mlvalues.is_ptr (Block.get_field block i) then
          debug_print_block (Block.get_field block i)
        else
          print_int (Mlvalues.long_val (Block.get_field block i));
        print_string ">";
        print_string " | "
      done;
     *)
    print_string "}"; 
    print_newline ()
  end

let debug_print_arr arr arr_end name =
  begin
    print_newline ();
    print_string name;
    print_string " :";
    print_newline ();
    for i = 0 to arr_end do  
      if Mlvalues.is_ptr arr.(i) then 
        debug_print_block (arr.(i))
      else begin
          print_int (Mlvalues.long_val arr.(i)) 
        end;
      print_string " | " 
    done;
    print_newline ()
  end

let debug_print_state () = 
  (if debug_pc then
     begin
       print_newline ();
       print_string "pc: "; 
       print_int (!pc) ; 
       print_newline ()
     end);
  (if debug then
     begin
       print_newline ();
       print_string "pc: "; 
       print_int (!pc);
       print_string ", acc: "; 
       if Mlvalues.is_ptr !Domain.acc then 
         debug_print_block !Domain.acc
       else 
         print_int (Mlvalues.long_val !Domain.acc);
       print_string ", env: ";
       if Mlvalues.is_ptr (!Domain.env) then
         debug_print_block (!Domain.env)
       else 
         print_int (Mlvalues.long_val !Domain.env);
       print_string ", Domain.sp: "; 
       print_int !Domain.sp;
       print_string ", extra args: ";
       print_int !extra_args;
       print_newline ();
       debug_print_arr Domain.stack (!Domain.sp-1) "stack";
       debug_print_arr !Domain.from_space
         (!Domain.heap_top - 1 - Domain.heap_start) "from_space"
     end);
  if debug_data
  then begin debug_print_arr Domain.global (!Domain.global_top - 1) "global";
             debug_print_arr Domain.data (!Domain.data_top - 1) "data"
       end
  
(* print_string " global: ";
 if Mlvalues.is_ptr (!global) then
 debug_print_block (!global);
 print_newline (); *)

let acc_n n = 
  Domain.acc := Domain.stack.(!Domain.sp - n - 1)

let push () = 
  push_stack !Domain.acc

let push_acc_n n = 
  push_stack !Domain.acc; 
  Domain.acc := Domain.stack.(!Domain.sp - n - 1)

let pop_n n =
  assert (!Domain.sp >= n);
  Domain.sp := !Domain.sp - n

let assign n =
  Domain.stack.(!Domain.sp-1-n) <- !Domain.acc; 
  Domain.acc := Mlvalues.val_long 0

let env_acc_n n = 
  Domain.acc := Block.get_field !Domain.env n

let push_env_acc_n n =
  push_stack !Domain.acc; 
  Domain.acc := Block.get_field !Domain.env n

let offsetclosure_n n =
  Domain.acc := Mlvalues.val_ptr (Mlvalues.ptr_val !Domain.env + n)

let pushoffsetclosure_n n =
  push_stack !Domain.acc;
  Domain.acc := Mlvalues.val_ptr (Mlvalues.ptr_val !Domain.env + n)

let get_field_n n =
  assert (Mlvalues.is_ptr !Domain.acc);
  Domain.acc := Block.get_field !Domain.acc n

let set_field_n n =
  assert (Mlvalues.is_ptr !Domain.acc);
  Block.set_field !Domain.acc n (pop_stack ()); Domain.acc := Block.unit 

let const_n n =
  Domain.acc := Mlvalues.val_long n

let pushconst_n n =
  push_stack !Domain.acc;
  Domain.acc := Mlvalues.val_long n

let appterm nargs n =
  for i = 0 to nargs - 1 do
    Domain.stack.(!Domain.sp - n + i) <- Domain.stack.(!Domain.sp - nargs + i) 
  done;
  pop_stack_ignore (n-nargs);
  pc := Mlvalues.long_val (Block.get_field !Domain.acc 0) - 1;
  Domain.env := !Domain.acc;
  extra_args := !extra_args + nargs - 1

let interp code =
  Domain.sp := 0;
  while !pc < Array.length code do
    if debug_opcode then (
      print_newline () ;
      print_string "opcode : " ; print_int code.(!pc)
    );
    debug_print_state ();
    begin
      match code.(!pc) with
      | 0 (* ACC0 *) -> acc_n 0
      | 1 (* ACC1 *) -> acc_n 1
      | 2 (* ACC2 *) -> acc_n 2
      | 3 (* ACC3 *) -> acc_n 3
      | 4 (* ACC4 *) -> acc_n 4
      | 5 (* ACC5 *) -> acc_n 5
      | 6 (* ACC6 *) -> acc_n 6
      | 7 (* ACC7 *) -> acc_n 7
      | 8 (* ACC *) -> acc_n @@ take_argument code
      | 9 (* PUSH *) -> push ()
      | 10 (* PUSHACC0 *) -> push ()
      | 11 (* PUSHACC1 *) -> push_acc_n 1
      | 12 (* PUSHACC2 *) -> push_acc_n 2
      | 13 (* PUSHACC3 *) -> push_acc_n 3
      | 14 (* PUSHACC4 *) -> push_acc_n 4
      | 15 (* PUSHACC5 *) -> push_acc_n 5
      | 16 (* PUSHACC6 *) -> push_acc_n 6
      | 17 (* PUSHACC7 *) -> push_acc_n 7
      | 18 (* PUSHACC *) -> push_acc_n @@ take_argument code
      | 19 (* POP *) -> pop_n @@ take_argument code
      | 20 (* ASSIGN *) -> assign @@ take_argument code
      | 21 (* ENVACC1 *) -> env_acc_n 1
      | 22 (* ENVACC2 *) -> env_acc_n 2
      | 23 (* ENVACC3 *) -> env_acc_n 3
      | 24 (* ENVACC4 *) -> env_acc_n 4
      | 25 (* ENVACC *) -> env_acc_n @@ take_argument code
      | 26 (* PUSHENVACC1 *) -> push_env_acc_n 1
      | 27 (* PUSHENVACC2 *) -> push_env_acc_n 2
      | 28 (* PUSHENVACC3 *) -> push_env_acc_n 3
      | 29 (* PUSHENVACC4 *) -> push_env_acc_n 4
      | 30 (* PUSHENVACC *) -> push_env_acc_n (take_argument code)
      | 31 (* PUSH-RETADDR *) -> let ofs = take_argument code in
                                 push_stack @@ Mlvalues.val_long !extra_args;
                                 push_stack !Domain.env;
                                 push_stack @@ Mlvalues.val_long ofs
      | 32 (* APPLY *) -> let args = take_argument code in
                          extra_args := args - 1;
                          pc := Mlvalues.long_val (Block.get_field !Domain.acc 0) - 1;
                          (* -1 annule l'incrémentation du pc en fin de boucle *)
                          Domain.env := !Domain.acc
      | 33 (* APPLY1 *) -> assert (Mlvalues.is_ptr !Domain.acc);
                           assert(let tag = Block.tag_val !Domain.acc in 
                                  tag = Block.closure_tag || 
                                    tag = Block.infix_tag
                             );
                           let arg = pop_stack () in
                           push_stack @@ Mlvalues.val_long !extra_args;
                           push_stack !Domain.env;
                           push_stack @@ Mlvalues.val_long !pc;
                           push_stack arg;
                           pc := Mlvalues.long_val (Block.get_field !Domain.acc 0) - 1;
                           Domain.env := !Domain.acc;
                           extra_args := 0
      | 34 (* APPLY2 *) -> let arg1 = pop_stack () in
                           let arg2 = pop_stack () in
                           push_stack @@ Mlvalues.val_long !extra_args;
                           push_stack @@ !Domain.env;
                           push_stack @@ Mlvalues.val_long !pc;
                           push_stack arg2;
                           push_stack arg1;
                           pc := Mlvalues.long_val (Block.get_field !Domain.acc 0) - 1;
                           Domain.env := !Domain.acc;
                           extra_args := 1
      | 35 (* APPLY3 *) -> let arg1 = pop_stack () in
                           let arg2 = pop_stack () in
                           let arg3 = pop_stack () in
                           push_stack @@ Mlvalues.val_long !extra_args;
                           push_stack !Domain.env;
                           push_stack @@ Mlvalues.val_long !pc;
                           push_stack arg3;
                           push_stack arg2;
                           push_stack arg1;
                           pc := Mlvalues.long_val (Block.get_field !Domain.acc 0) - 1;
                           Domain.env := !Domain.acc;
                           extra_args := 2
      | 36 (* APPTERM *) -> let nbargs = take_argument code in 
                            let n = take_argument code in 
                            appterm nbargs n
      | 37 (* APPTERM1 *) -> appterm 1 (take_argument code) 
      | 38 (* APPTERM2 *) -> appterm 2 (take_argument code)
      | 39 (* APPTERM3 *) -> appterm 3 (take_argument code)
      | 40 (* RETURN *) -> let n = take_argument code in
                           Domain.sp := !Domain.sp - n; 
                           if !extra_args <= 0 
                           then 
                             begin
                               pc := Mlvalues.long_val @@ pop_stack ();
                               Domain.env := pop_stack ();
                               extra_args := Mlvalues.long_val @@ pop_stack ()
                             end
                           else 
                             begin
                               decr extra_args;
                               pc := (Mlvalues.long_val @@
                                        Block.get_field !Domain.acc 0) - 1;
                               Domain.env := !Domain.acc
                             end
      | 41 (* RESTART *) ->
         let size = Block.size (Mlvalues.ptr_val !Domain.env) - 2 in
         for i = 0 to size - 1 do 
           push_stack @@ Block.get_field !Domain.env (i+2)
         done;
         Domain.env := Block.get_field !Domain.env 1;
         extra_args := !extra_args + size
      | 42 (* GRAB *) -> let n = take_argument code in
                         if !extra_args >= n 
                         then extra_args := !extra_args - n
                         else 
                           begin 
                             Domain.acc := Alloc.make_block
                                             Block.closure_tag
                                             (!extra_args + 3);
                             Block.set_field !Domain.acc 0 (Mlvalues.val_long (!pc - 2));
                             Block.set_field !Domain.acc 1 !Domain.env;
                             for i = 0 to !extra_args do
                               Block.set_field !Domain.acc (i+2) (pop_stack ())
                             done;
                             pc := Mlvalues.long_val (pop_stack ());
                             Domain.env := pop_stack ();
                             extra_args := Mlvalues.long_val @@ pop_stack ()
                           end
      | 43 (* CLOSURE *) -> let n = take_argument code in
                            let addr = take_argument code in
                            if n > 0 then push_stack !Domain.acc;
                            Domain.acc := Alloc.make_closure addr (n+1);
                            for i = 1 to n do
                              Block.set_field !Domain.acc i (pop_stack ())
                            done
      | 44 (* CLOSUREREC *) -> 
         let f = take_argument code in
         let v = take_argument code in
         let o = take_argument code in
         if v > 0 then push_stack !Domain.acc;
         let closure_size = (2 * f) - 1 + v in  
         Domain.acc := Alloc.make_block Block.closure_tag closure_size;  
         for i = 1 to f - 1 do 
           Block.set_field !Domain.acc (2 * i - 1) (
               Block.make_header Block.infix_tag (2 * i)
             );
           Block.set_field !Domain.acc (2 * i) (
               Mlvalues.val_long @@ take_argument code
             )
         done;
         for i = 0 to v - 1 do 
           Block.set_field !Domain.acc (i + 2 * f - 1) (pop_stack ())
         done;
         Block.set_field !Domain.acc 0 (Mlvalues.val_long o);
         push_stack !Domain.acc;
         for i = 1 to f - 1 do
           push_stack (Mlvalues.val_ptr ((Mlvalues.ptr_val !Domain.acc) + (2 * i)))
         done
      | 45 (* OFFSETCLOSUREM2 *) -> offsetclosure_n (-2)
      | 46 (* OFFSETCLOSURE0 *) -> offsetclosure_n 0
      | 47 (* OFFSETCLOSURE2 *) -> offsetclosure_n 2
      | 48 (* OFFSETCLOSURE *) -> offsetclosure_n @@ take_argument code
      | 49 (* PUSHOFFSETCLOSUREM2 *) -> pushoffsetclosure_n (-2)
      | 50 (* PUSHOFFSETCLOSURE0 *) -> pushoffsetclosure_n 0
      | 51 (* PUSHOFFSETCLOSURE2 *) -> pushoffsetclosure_n 2
      | 52 (* PUSHOFFSETCLOSURE *) -> pushoffsetclosure_n @@ take_argument code
      | 53 (* GETGLOBAL *) -> let n = take_argument code in
                              Domain.acc := Block.get_global n
      | 54 (* PUSHGETGLOBAL *) -> push_stack !Domain.acc;
                                  let n = take_argument code in
                                  Domain.acc := Block.get_global n
      | 55 (* GETGLOBALFIELD *) -> let n = take_argument code in
                                   let p = take_argument code in
                                   Domain.acc := Block.get_field Domain.global.(n) p
      | 56 (* PUSHGETGLOBALFIELD *) -> push_stack !Domain.acc;
                                       let n = take_argument code in
                                       let p = take_argument code in
                                       Domain.acc := Block.get_field Domain.global.(n) p
      | 57 (* SETGLOBAL *) -> let n = take_argument code in
                              Block.set_global n !Domain.acc;
                              Domain.acc := Block.unit
      | 58 (* ATOM0 *) -> Domain.acc := Alloc.make_block 0 0
      | 59 (* ATOM *) -> let tag = take_argument code in
                         Domain.acc := Alloc.make_block tag 0
      | 60 (* PUSHATOM0 *) -> push_stack !Domain.acc;
                              Domain.acc := Alloc.make_block 0 0
      | 61 (* PUSHATOM *) -> push_stack !Domain.acc;
                             let tag = take_argument code in
                             Domain.acc := Alloc.make_block tag 0
      | 62 (* MAKEBLOCK *) -> let sz = take_argument code in
                              let tag = take_argument code in
                              let blk = Alloc.make_block tag sz in
                              Block.set_field blk 0 !Domain.acc;
                              for i = 1 to sz - 1 do 
                                Block.set_field blk i (pop_stack ())
                              done;
                              Domain.acc := blk
      | 63 (* MAKEBLOCK1 *) -> let tag = take_argument code in
                               let blk = Alloc.make_block tag 1 in
                               Block.set_field blk 0 !Domain.acc;
                               Domain.acc := blk
      | 64 (* MAKEBLOCK2 *) -> let tag = take_argument code in
                               let blk = Alloc.make_block tag 2 in
                               Block.set_field blk 0 !Domain.acc;
                               Block.set_field blk 1 (pop_stack ());
                               Domain.acc := blk
      | 65 (* MAKEBLOCK3 *) -> let tag = take_argument code in
                               let blk = Alloc.make_block tag 3 in
                               Block.set_field blk 0 !Domain.acc;
                               Block.set_field blk 1 (pop_stack ());
                               Block.set_field blk 2 (pop_stack ());
                               Domain.acc := blk

      (* 66 MAKEFLOATBLOCK *)

      | 67 (* GETFIELD0 *) -> get_field_n 0
      | 68 (* GETFIELD1 *) -> get_field_n 1
      | 69 (* GETFIELD2 *) -> get_field_n 2
      | 70 (* GETFIELD3 *) -> get_field_n 3
      | 71 (* GETFIELD *) -> get_field_n @@ take_argument code

      (* GETFLOATFIELD (opInput.code: 72) *)

      | 73 (* SETFIELD0 *) -> set_field_n 0
      | 74 (* SETFIELD1 *) -> set_field_n 1
      | 75 (* SETFIELD2 *) -> set_field_n 2
      | 76 (* SETFIELD3 *) -> set_field_n 3
      | 77 (* SETFIELD *) -> set_field_n @@ take_argument code

      (* 78 SETFLOATFIELD *)

      | 79 (* VECTLENGTH *) -> Domain.acc := Mlvalues.val_long @@
                                               Block.size (Mlvalues.ptr_val !Domain.acc)
      | 80 (* GETVECTITEM *) -> let n = Mlvalues.long_val @@ pop_stack () in
                                Domain.acc := Block.get_field !Domain.acc n
      | 81 (* SETVECTITEM *) -> let n = pop_stack () in
                                let v = pop_stack () in
                                Block.set_bytes !Domain.acc (Mlvalues.long_val n) v;
                                Domain.acc := Block.unit
      | 82 (* GETSTRINGCHAR *) -> let n = pop_stack () in
                                  Domain.acc := Block.get_bytes
                                                  !Domain.acc
                                                  (Mlvalues.long_val n)
      | 83 (* SETBYTESCHAR *) -> let n = pop_stack () in
                                 let v = pop_stack () in
                                 Block.set_bytes !Domain.acc (Mlvalues.long_val n) (
                                     pop_stack ()
                                   );
                                 Domain.acc := Block.unit
      | 84 (* BRANCH *) -> let n = take_argument code in 
                           pc := n - 1
      | 85 (* BRANCHIF *) -> let n = take_argument code in 
                             if not (Mlvalues.is_ptr !Domain.acc) &&
                                  Mlvalues.long_val !Domain.acc <> 0 
                             then pc := n - 1 (* attention à l'adresse zéro *)
      | 86 (* BRANCHIFNOT *) -> let n = take_argument code in 
                                if not (Mlvalues.is_ptr !Domain.acc)
                                   && Mlvalues.long_val !Domain.acc = 0 
                                then pc := n - 1  (* attention à l'adresse zéro *)
      | 87 (* SWITCH *) ->   
         let n = take_argument code in 
         if Mlvalues.is_ptr !Domain.acc 
         then pc := let idx = Block.tag (Mlvalues.ptr_val !Domain.acc) in
                    let ofs = 2 * (n + idx) + 1 in
                    ofs - 1
         else pc := Mlvalues.long_val !Domain.acc
      | 88 (* BOOLNOT *) ->
         Domain.acc := Mlvalues.val_long @@ Prims.bnot (Mlvalues.long_val !Domain.acc)
      | 89 (* PUSHTRAP *) ->
         let ofs = take_argument code in
         push_stack @@ Mlvalues.val_long !extra_args;
         push_stack !Domain.env;
         push_stack @@ Mlvalues.val_long !trap_sp;
         push_stack @@ Mlvalues.val_long (!pc - 1 + ofs);
         trap_sp := !Domain.sp
      | 90 (* POPTRAP *) -> 
         pop_stack_ignore 1;
         trap_sp := Mlvalues.long_val @@ pop_stack ();
         pop_stack_ignore 2
      | 91 (* RAISE *) -> 
         if !trap_sp = 0 then begin 
             print_string "Exception, acc = ";
             print_int @@ Mlvalues.long_val !Domain.acc;
             print_newline () end
         else begin
             Domain.sp := !trap_sp;
             pc := Mlvalues.long_val @@ pop_stack ();
             trap_sp := Mlvalues.long_val @@ pop_stack ();
             Domain.env := pop_stack ();
             extra_args := Mlvalues.long_val @@ pop_stack ()
           end
        
      | 92 (* CHECK-SIGNALS *) -> ()

      | 93 (* C-CALL1 *) ->
         let p = take_argument code in
         push_stack !Domain.env; 
         Domain.acc := (match p with
                        | 0 -> Call.n2t_print_int_code     !Domain.acc
                        | 1 -> Call.n2t_print_newline_code !Domain.acc
                        | 2 -> Call.n2t_print_char_code    !Domain.acc
                        | 3 -> Call.n2t_print_string_code  !Domain.acc        
                        | 4 -> Call.n2t_array_length_code  !Domain.acc
                        | 5 -> Call.caml_fresh_oo_id_code  !Domain.acc
                        | 6 -> Call.caml_array_concat_code !Domain.acc  
                        | _ -> Call.not_available ());
         pop_stack_ignore 1
      | 94 (* C-CALL2 *) -> 
         let p = take_argument code in
         let v = pop_stack () in
         push_stack !Domain.env; 
         Domain.acc := (match p with
                        | 0 -> Call.caml_make_vect_code !Domain.acc v
                        | 1 -> Call.caml_array_get_addr_code !Domain.acc v
                        | 2 -> Call.caml_greaterequal_code !Domain.acc v
                        | 3 -> Call.caml_lessequal_code !Domain.acc v
                        | 4 -> Call.caml_lessthan_code !Domain.acc v
                        | 5 -> Call.caml_int_compare_code !Domain.acc v
                        | 6 -> Call.caml_array_append_code !Domain.acc v                     
                        | _ -> Call.not_available ());
         pop_stack_ignore 1
      | 95 (* C-CALL3 *) ->
         let p = take_argument code in
         let v1 = pop_stack () in
         let v2 = pop_stack () in
         push_stack !Domain.env; 
         Domain.acc := (match p with
                        | 0 -> Call.caml_array_set_addr_code !Domain.acc v1 v2
                        | 1 -> Call.caml_array_sub_code !Domain.acc v1 v2
                        | _ -> Call.not_available ());
         Domain.env := pop_stack ()
      | 96 (* C-CALL4 *) -> 
         let p = take_argument code in
         let v1 = pop_stack () in
         let v2 = pop_stack () in
         let v3 = pop_stack () in
         push_stack !Domain.env; 
         Domain.acc := (match p with
                        | _ -> Call.not_available ());
         Domain.env := pop_stack ()
      | 97 (* C-CALL5 *) -> 
         let p = take_argument code in
         let v1 = pop_stack () in
         let v2 = pop_stack () in
         let v3 = pop_stack () in
         let v4 = pop_stack () in
         push_stack !Domain.env; 
         Domain.acc := (match p with
                        | 0 -> Call.caml_array_blit_code !Domain.acc v1 v2 v3 v4
                        | _ -> Call.not_available ());
         Domain.env := pop_stack ()
      (* 98 C-CALLN *)
      | 99  (* CONST0 *) -> const_n 0
      | 100 (* CONST1 *) -> const_n 1
      | 101 (* CONST2 *) -> const_n 2
      | 102 (* CONST3 *) -> const_n 3
      | 103 (* CONSTINT *) -> const_n @@ take_argument code
      | 104 (* PUSHCONST0 *) -> pushconst_n 0
      | 105 (* PUSHCONST1 *) -> pushconst_n 1
      | 106 (* PUSHCONST2 *) -> pushconst_n 2
      | 107 (* PUSHCONST3 *) -> pushconst_n 3
      | 108 (* PUSHCONSTINT *) -> pushconst_n @@ take_argument code
      | 109 (* NEGINT *) ->
         Domain.acc := Mlvalues.val_long @@
                         Prims.negint (Mlvalues.long_val !Domain.acc)
      | 110 (* ADDINT *) ->
         Domain.acc := Mlvalues.val_long @@
                         Prims.addint
                           (Mlvalues.long_val !Domain.acc)
                           (Mlvalues.long_val (pop_stack ()))
      | 111 (* SUBINT *) ->
         Domain.acc := Mlvalues.val_long @@
                         Prims.subint
                           (Mlvalues.long_val !Domain.acc)
                           (Mlvalues.long_val (pop_stack ()))
      | 112 (* MULINT *) ->
         Domain.acc := Mlvalues.val_long @@
                         Prims.mulint
                           (Mlvalues.long_val !Domain.acc)
                           (Mlvalues.long_val (pop_stack ()))
      | 113 (* DIVINT *) ->
         Domain.acc := Mlvalues.val_long @@
                         Prims.divint
                           (Mlvalues.long_val !Domain.acc)
                           (Mlvalues.long_val (pop_stack ())) 
      | 114 (* MODINT *) ->
         Domain.acc := Mlvalues.val_long @@
                         Prims.modint
                           (Mlvalues.long_val !Domain.acc)
                           (Mlvalues.long_val (pop_stack ()))  
      | 115 (* ANDINT *) ->
         Domain.acc := Mlvalues.val_long @@
                         Prims.andint
                           (Mlvalues.long_val !Domain.acc)
                           (Mlvalues.long_val (pop_stack ())) 
      | 116 (* ORINT  *) ->
         Domain.acc := Mlvalues.val_long @@
                         Prims.orint
                           (Mlvalues.long_val !Domain.acc)
                           (Mlvalues.long_val (pop_stack ()))  
      | 117 (* XORINT *) ->
         Domain.acc := Mlvalues.val_long @@
                         Prims.xorint
                           (Mlvalues.long_val !Domain.acc)
                           (Mlvalues.long_val (pop_stack ())) 
      | 118 (* LSLINT *) ->
         Domain.acc := Mlvalues.val_long @@
                         Prims.lslint
                           (Mlvalues.long_val !Domain.acc)
                           (Mlvalues.long_val (pop_stack ())) 
      | 119 (* LSRINT *) ->
         Domain.acc := Mlvalues.val_long @@
                         Prims.lsrint
                           (Mlvalues.long_val !Domain.acc)
                           (Mlvalues.long_val (pop_stack ())) 
      | 120 (* ASRINT *) ->
         Domain.acc := Mlvalues.val_long @@
                         Prims.asrint
                           (Mlvalues.long_val !Domain.acc)
                           (Mlvalues.long_val (pop_stack ()))
      | 121 (* EQ *) ->
         Domain.acc := Mlvalues.val_long @@
                         Prims.eq
                           (Mlvalues.long_val !Domain.acc)
                           (Mlvalues.long_val (pop_stack ()))
      | 122 (* NEQ *) ->
         Domain.acc := Mlvalues.val_long @@
                         Prims.neq
                           (Mlvalues.long_val !Domain.acc)
                           (Mlvalues.long_val (pop_stack ())) 
      | 123 (* LTINT *) ->
         Domain.acc := Mlvalues.val_long @@
                         Prims.ltint
                           (Mlvalues.long_val !Domain.acc)
                           (Mlvalues.long_val (pop_stack ()))  
      | 124 (* LEINT *) ->
         Domain.acc := Mlvalues.val_long @@
                         Prims.leint
                           (Mlvalues.long_val !Domain.acc)
                           (Mlvalues.long_val (pop_stack ()))
      | 125 (* GTINT *) ->
         Domain.acc := Mlvalues.val_long @@
                         Prims.gtint
                           (Mlvalues.long_val !Domain.acc)
                           (Mlvalues.long_val (pop_stack ())) 
      | 126 (* GEINT *) ->
         Domain.acc := Mlvalues.val_long @@
                         Prims.geint
                           (Mlvalues.long_val !Domain.acc)
                           (Mlvalues.long_val (pop_stack ()))
      | 127 (* OFFSETINT *) -> let ofs = take_argument code in 
                               Domain.acc := Mlvalues.val_long @@
                                               Prims.addint
                                                 (Mlvalues.long_val !Domain.acc)
                                                 ofs
      | 128 (* OFFSETREF *) -> let ofs = take_argument code in
                               let old = Block.get_field !Domain.acc 0 in
                               Block.set_field !Domain.acc 0
                                 (Mlvalues.val_long @@
                                    Prims.addint (Mlvalues.long_val old) ofs);
                               Domain.acc := Block.unit
      | 129 (* ISINT *) -> Domain.acc := Mlvalues.val_long @@
                                           Prims.isint !Domain.acc
      | 130 (* GETMETHOD *) -> (* todo *)
         let x = pop_stack () in 
         let y = Block.get_field x 0 in
         Domain.acc := Block.get_field y
                         (Mlvalues.long_val @@ !Domain.acc)
      | 131 (* BEQ *) -> 
         let v = take_argument code in
         let ofs = take_argument code in
         if Prims.compare_imm v (Mlvalues.long_val !Domain.acc) = 0
         then pc := ofs - 1
      | 132 (* BNEQ *) -> 
         let v = take_argument code in
         let ofs = take_argument code in
         if Prims.compare_imm v (Mlvalues.long_val !Domain.acc) <> 0
         then pc := ofs - 1
      | 133 (* BLTINT *) ->
         let v = take_argument code in
         let ofs = take_argument code in
         if Prims.compare_imm v (Mlvalues.long_val !Domain.acc) < 0
         then pc := ofs - 1
      | 134 (* BLEINT *) ->
         let v = take_argument code in
         let ofs = take_argument code in
         if Prims.compare_imm v (Mlvalues.long_val !Domain.acc) <= 0
         then pc := ofs - 1
      | 135 (* BGTINT *) ->
         let v = take_argument code in
         let ofs = take_argument code in
         if Prims.compare_imm v (Mlvalues.long_val !Domain.acc) > 0
         then pc := ofs - 1
      | 136 (* BGEINT *) ->
         let v = take_argument code in
         let ofs = take_argument code in
         if Prims.compare_imm v (Mlvalues.long_val !Domain.acc) >= 0
         then pc := ofs - 1
      | 137 (* ULTINT *) -> Domain.acc := Mlvalues.val_long @@
                                            Prims.ultint
                                              (Mlvalues.long_val !Domain.acc)
                                              (Mlvalues.long_val (pop_stack ()))
      | 138 (* UGEINT *) -> Domain.acc := Mlvalues.val_long @@
                                            Prims.ugeint
                                              (Mlvalues.long_val !Domain.acc)
                                              (Mlvalues.long_val (pop_stack ()))
      | 139 (* BULTINT *) -> let v = take_argument code in 
                             let ofs = take_argument code in
                             if Prims.ultint v (Mlvalues.long_val !Domain.acc) = 1
                             then pc := ofs - 1

      | 140 (* BUGEINT *) -> let v = take_argument code in 
                             let ofs = take_argument code in
                             if Prims.ugeint v (Mlvalues.long_val !Domain.acc) = 1
                             then pc := ofs - 1
      (* GETPUBMET *)
      (* GETDYNMET *)

      | 143 (* STOP *) -> pc := Array.length code

      (* EVENT *)
      (* BREAK *)

      | _ -> print_string "unknown opcode : ";
             print_int code.(!pc);
             print_newline ();
             exit 1
    end;
    incr pc
  done
