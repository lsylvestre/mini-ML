CAMLC=ocamlc
FLAGS= -nopervasives -no-alias-deps -open Pervasives
MLFILES= # foo.ml
CFILES=primitives.c
EXE=a.out
INCLUDES=-I ../stdlib $(foreach f,$(MLFILES),-I $(dir $(f)))
STDLIB_SRC= ../stdlib/pervasives.ml # stdlib/array.ml stdlib/list.ml
OBJS= $(subst .ml,.cmo,$(STDLIB_SRC) $(MLFILES))

all: clean-cmo $(OBJS)
	$(CAMLC) $(FLAGS) $(INCLUDES) -custom -o $(EXE) $(CFILES) $(OBJS)

bytecode:	clean-all all clean

	# 1) néttoie les anciens résidu # 2) compile (avec interop) # 3) néttoie tout sauf les .cmo

.SUFFIXES: .c .o .ml .mli .cmo .cmi .cmx

.c.o:
	cc -c -I`ocamlc -where` $<

../stdlib/pervasives.cmo:	../stdlib/pervasives.ml 
	$(CAMLC) -nopervasives -no-alias-deps $(INCLUDES) -c $<

.ml.cmo:
	$(CAMLC) $(INCLUDES) $(FLAGS) -c $<

.mli.cmi:
	$(CAMLC) $(INCLUDES) $(FLAGS) -c $<

clean:
	rm -rf  *~ *.cm[ix] *.o $(EXE)

clean-cmo:
	rm -rf *.cmo $(OBJS)

clean-all:	clean clean-cmo
