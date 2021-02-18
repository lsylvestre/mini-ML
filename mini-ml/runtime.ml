(**************************************************************************)
(*                                                                        *)
(*         PSTL : OCaml sur la plate-forme Nand2Tetris (2020)             *)
(*                                                                        *)      
(*           Loïc SYLVESTRE              Pablito BELLO                    *)
(*           loic.sylvestre@etu.upmc.fr  pablito.bello@etu.upmc.fr        *)
(*                                                                        *)  
(**************************************************************************)

let load_file f =
  let ic = open_in f in
  let n = in_channel_length ic in
  let s = Bytes.create n in
  really_input ic s 0 n;
  close_in ic;
  (s)

let mv f dst =
  let s = load_file f in 
  let oc = open_out dst in
  Printf.fprintf oc "%s" (Bytes.to_string s);
  close_out oc

let link_test_file dir = 
  let oc = open_out (Filename.concat dir ("Main.tst")) in
  Printf.fprintf oc "%s\n" "load; repeat 25000000 { vmstep; }";
  close_out oc

let link_runtime dir = 
  mv "stdlib/ML_Internal.vm" (Filename.concat dir "ML_Internal.vm");
  mv "stdlib/Main.vm" (Filename.concat dir "Main.vm")
(* mv "stdlib/ML_array.vm" (Filename.concat dir "ML_array.vm");
  mv "stdlib/ML_pervasives.vm" (Filename.concat dir "ML_pervasives.vm");
  mv "stdlib/ML_string.vm" (Filename.concat dir "ML_string.vm");
  mv "stdlib/ML_obj.vm" (Filename.concat dir "ML_obj.vm")
 *)
let init dir = 
  link_test_file dir;
  link_runtime dir


  
module PrimTypes = struct
  open Types

  let v () =
    Tvar (V.create ())

  let ty_internal_exit = 
    Tarrow(Tint,v())

  let ty_internal_array_length = 
    Tarrow(v(),Tint)

  let ty_internal_array_get = 
    Tarrow(v (),Tarrow (Tint,v ()))

  let ty_internal_array_set = 
    Tarrow(v (),Tarrow (Tint,Tarrow(v (),Tunit)))
  let ty_internal_array_make = 
    Tarrow(Tint,Tarrow(v(),v()))
  let ty_internal_array_create_uninitialized =
    Tarrow(Tint,v())

  let ty_internal_pair =
    let a,b = v (),v () in 
    Tarrow(a,Tarrow(b,Tproduct(a,b)))

  let ty_internal_fst = 
    let a,b = v (),v () in 
    Tarrow(Tproduct(a,b),a)

  let ty_internal_snd = 
    let a,b = v (), v() in 
    Tarrow(Tproduct(a,b),b)

  let ty_internal_ignore = 
    let a = v () in 
    Tarrow(a,Tunit)

  let ty_exit = 
    ty_internal_exit

  let ty_failwith =
    Tarrow(Tstring,v ())

  let ty_ref = 
    let a = v () in 
    Tarrow(a,Tref a)

  let ty_ref_contents = 
    let a = v () in 
    Tarrow(Tref a,a)

  let ty_ref_set_contents = 
    let a = v () in 
    Tarrow(Tref a,(Tarrow(a,Tunit)))

  let ty_incr = 
    Tarrow(Tref (v ()),Tunit)

  let ty_decr = 
    Tarrow(Tref (v ()),Tunit)


  let ty_fst = 
    let a,b = v (),v () in 
    Tarrow(Tproduct(a,b),a)

  let ty_snd = 
    let a,b = v (), v() in 
    Tarrow(Tproduct(a,b),b)

  let ty_print_char =
    Tarrow(Tchar,Tunit)

  let ty_print_string = 
    Tarrow(Tstring,Tunit)

  let ty_print_int =
    Tarrow(Tint,Tunit)

  let ty_print_newline =
    Tarrow(Tunit,Tunit)

  let ty_array_length = 
    Tarrow(Tarray (v ()),Tint)

  let ty_array_set = 
    let a = v () in
    Tarrow(Tarray a,Tarrow(Tint,Tarrow(a,Tunit)))

  let ty_array_get = 
    let a = v () in
    Tarrow(Tarray a,Tarrow(Tint,a))

  let ty_array_create_uninitialized =
    Tarrow(Tint,Tarray (v ()))

  let ty_array_make =
    let a = v () in
    Tarrow(Tint,Tarrow(a,Tarray a))

  let ty_array_create =
    let a = v () in
    Tarrow(Tint,Tarrow(a,Tarray a))

  let ty_string_length =
    Tarrow(Tstring,Tint)

  let ty_string_get =
    Tarrow(Tstring,Tarrow(Tint,Tchar))

  let ty_string_set =
    Tarrow(Tstring,Tarrow(Tint,Tarrow(Tchar,Tunit)))

  let ty_obj_magic =
    Tarrow (v (), v ())

  let ty_abs = Tarrow (Tint, Tint)

  let ty_concat = Tarrow (Tstring, Tarrow (Tstring, Tstring))

  let ty_string_of_int = Tarrow (Tint, Tstring)

  let ty_int_of_char = Tarrow (Tchar, Tint)

  let ty_not = Tarrow(Tbool,Tbool)
end

let primitives =
  let open PrimTypes in
  let ml_internal = 
    [("Internal.exit",             "Internal.exit",             ty_internal_exit);
     ("Internal.array_length",     "Internal.array_length",     ty_internal_array_length);
     ("Internal.array_get",        "Internal.array_get",        ty_internal_array_get);
     ("Internal.array_set",        "Internal.array_set",        ty_internal_array_set);
     ("Internal.array_make",       "Internal.array_make",       ty_internal_array_make);
     ("Internal.array_create_uninitialized", "Internal.array_create_uninitialized", ty_internal_array_create_uninitialized);
     ("Internal.print_char",       "Internal.print_char",       ty_print_char);
     ("Internal.print_char_array", "Internal.print_char_array", ty_print_string); 
     ("Internal.print_int",        "Internal.print_int",        ty_print_int);
     ("Internal.print_newline",    "Internal.print_newline",    ty_print_newline);
     ("Internal.make_pair",        "Internal.make_pair",        ty_internal_pair); 
     ("Internal.fst",              "Internal.left",             ty_fst);
     ("Internal.snd",              "Internal.right",            ty_snd);
     ("Internal.free",             "Internal.free",             ty_internal_ignore);
     ("Internal.obj_magic",        "Internal.obj_magic",        ty_obj_magic);
     ("Obj.magic",                 "Internal.obj_magic",        ty_obj_magic)] in
  let openned_ml_pervasives =
    [("exit",             "Pervasives.exit",             ty_exit);
     ("failwith",         "Pervasives.failwith",         ty_failwith);
     ("ref",              "Pervasives.ref",              ty_ref);
     ("ref_contents",     "Pervasives.ref_contents",     ty_ref_contents);
     ("ref_set_contents", "Pervasives.ref_set_contents", ty_ref_set_contents);
     ("incr",             "Pervasives.incr",             ty_incr);
     ("decr",             "Pervasives.decr",             ty_decr);
     ("not",              "Pervasives.not",              ty_not);
     ("fst",              "Pervasives.fst",              ty_fst);
     ("snd",              "Pervasives.snd",              ty_snd);
     ("print_char",       "Pervasives.print_char",       ty_print_char);
     ("print_string",     "Pervasives.print_string",     ty_print_string); 
     ("print_int",        "Pervasives.print_int",        ty_print_int);
     ("print_newline",    "Pervasives.print_newline",    ty_print_newline);
     ("abs",              "Pervasives.abs",              ty_abs);
     ("(^)",              "Pervasives.(^)",              ty_concat);
     ("string_of_int",    "Pervasives.string_of_int",    ty_string_of_int);
     ("int_of_char",      "Pervasives.int_of_char",      ty_int_of_char);
     ("ignore",           "Pervasives.ignore",           ty_internal_ignore)] in
  openned_ml_pervasives @ ml_internal
