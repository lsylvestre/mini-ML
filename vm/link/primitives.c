#include <stdio.h>
#define CAML_NAME_SPACE
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>

CAMLprim value n2t_print_int(value v){
  CAMLparam1(v);
  int n = Int_val(v);
  printf ("%d",n);
  CAMLreturn (Val_unit);
}

CAMLprim value n2t_print_char(value v){
  CAMLparam1(v);
  int n = Int_val(v);
  printf ("%c",n);
  CAMLreturn (Val_unit);
}

CAMLprim value n2t_print_newline(value v){
  CAMLparam1(v);
  /* TODO */
  printf ("\n");
  CAMLreturn (Val_unit);
}

CAMLprim value n2t_print_string(value v){
  CAMLparam1(v);
  /* TODO */
  printf ("TODO");
  CAMLreturn (Val_unit);
}

CAMLprim value n2t_make_vect(value v1, value v2){
  CAMLparam2(v1,v2);
  /* TODO */
  printf ("TODO");
  CAMLreturn (Val_unit); // devra retourner un bloc
}

CAMLprim value n2t_array_length(value v){
  CAMLparam1(v);
  // TODO 
  printf ("TODO");
  CAMLreturn (Val_int(0)); // devra retourner la taille du tableau
}

CAMLprim value n2t_array_sub(value v1, value v2, value v3){
  CAMLparam3(v1,v2,v3);
  /* TODO */
  printf ("TODO");
  CAMLreturn (Val_unit); // devra retourner un bloc
}

CAMLprim value n2t_array_get_addr(value v1, value v2){
  CAMLparam2(v1,v2);
  /* TODO */
  printf ("TODO");
  CAMLreturn (Val_unit); // devra retourner un élément du tableau
}

CAMLprim value n2t_array_set_addr(value v1, value v2, value v3){
  CAMLparam3(v1,v2,v3);
  /* TODO */
  printf ("TODO");
  CAMLreturn (Val_unit);
}

