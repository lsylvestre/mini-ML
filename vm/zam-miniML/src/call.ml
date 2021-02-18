(**************************************************************************)
(*                                                                        *)
(*         PSTL : OCaml sur la plate-forme Nand2Tetris (2020)             *)
(*                                                                        *)      
(*           Loïc SYLVESTRE              Pablito BELLO                    *)
(*           loic.sylvestre@etu.upmc.fr  pablito.bello@etu.upmc.fr        *)
(*                                                                        *)  
(**************************************************************************)

let not_available () = failwith "The external function is not available"

(* ************************ CALL1 ***************************** *)

let n2t_print_int = 0

let n2t_print_int_code v = 
  Pervasives.print_int (Mlvalues.long_val v); 
  Block.unit

let n2t_print_newline = 1
let n2t_print_newline_code _ = 
  Pervasives.print_newline (); 
  Block.unit

let n2t_print_char = 2
let n2t_print_char_code v = 
  Pervasives.print_char (Obj.magic (Mlvalues.long_val v)); 
  Block.unit

let n2t_print_string = 3
let n2t_print_string_code v = 
  let p = Mlvalues.ptr_val v in
  let z = Block.size p in
  for i = 0 to z - 1 do

    Pervasives.print_char (Obj.magic (Mlvalues.long_val (Block.get (p+i+1))))
  done; 
  Block.unit

(* [array_leng a] renvoie la taille du tableau a *)
let n2t_array_length = 4

let n2t_array_length_code a =
  Mlvalues.val_long (Block.size (Mlvalues.ptr_val a))

let caml_fresh_oo_id = 5 (* définition d'exception *)
let caml_fresh_oo_id_code _ = Block.unit

let caml_array_concat = 6 
let caml_array_concat_code v1 = failwith "todo" 
(* ************************ CALL2 ***************************** *)

(* [n2t_make_vect z x] renvoie un bloc (tableau) de taille z *)
(* dont tout les champs sont initialisé à x *)
let caml_make_vect = 0

let caml_make_vect_code sz init =
  let n = Mlvalues.long_val sz in
  if n < 0 then failwith "caml_make_vect" else
    let arr = Alloc.make_block 0 n in (* array : tag 0 *)
    for i = 0 to n - 1 do
      Block.set_field arr i init
    done;
    arr

(* [array_get a n] renvoie le n-ième élement du tableau a *)
let caml_array_get = 1
let caml_array_unsafe_get = 1
let caml_array_get_addr = 1

let caml_array_get_addr_code a v =
  Block.get_field a (Mlvalues.long_val v)

let caml_greaterequal = 2
let caml_greaterequal_code v1 v2 = Mlvalues.val_long @@
                                     Prims.geint
                                       (Mlvalues.long_val v1)
                                       (Mlvalues.long_val v2)

let caml_lessequal = 3
let caml_lessequal_code v1 v2 = Mlvalues.val_long @@
                                  Prims.leint
                                    (Mlvalues.long_val v1)
                                    (Mlvalues.long_val v2)
let caml_lessthan = 4
let caml_lessthan_code v1 v2 = Mlvalues.val_long @@
                                  Prims.ltint
                                    (Mlvalues.long_val v1)
                                    (Mlvalues.long_val v2)
let caml_int_compare = 5
let caml_compare = 5
let caml_int_compare_code v1 v2 = Mlvalues.val_long @@
                                    Prims.compare_imm
                                      (Mlvalues.long_val v1)
                                      (Mlvalues.long_val v2)

let caml_array_append = 6
let caml_array_append_code v1 v2 = failwith "todo"



(* ************************ CALL3 ***************************** *)

(* [array_get a n x] affecte x au n-ième élement du tableau a *)
let caml_array_set = 0
let caml_array_unsafe_set = 0
let caml_array_set_addr = 0

let caml_array_set_addr_code a v x =
  Block.set_field a (Mlvalues.long_val v) x; 
  Block.unit

(* [array_sub a ofs len] ... *)
let caml_array_sub = 1

let caml_array_sub_code a v1 v2 =
  let ofs = Mlvalues.long_val v1 in
  let len = Mlvalues.long_val v2 in
  if ofs < 0 || len < 0 || ofs > Block.size (Mlvalues.ptr_val a) - len
  then failwith "caml_array_sub" else
    let arr = Alloc.make_block 0 len in (* array : tag 0 *)
    for i = ofs to ofs + len - 1 do
      Block.set_field arr i (Block.get_field a i)
    done;
    arr


(* ************************ CALL3 ***************************** *)

let caml_array_blit = 0
let caml_array_blit_code _ _ _ _ _ = failwith "todo"
