ZAM-MINIML=../zam-miniML/

CC=ocamlc -w=-26 -I $(ZAM-MINIML) -I $(ZAM-MINIML)src 
EXE=zam.exe
MLVALUES=mlvalues.ml

all:
	$(CC) -c $(MLVALUES)
	$(CC) -c $(ZAM-MINIML)src/prims.ml
	$(CC) -c $(ZAM-MINIML)src/domain.ml
	$(CC) -c $(ZAM-MINIML)src/block.ml
	$(CC) -c $(ZAM-MINIML)src/data.ml
	$(CC) -c $(ZAM-MINIML)src/gc.ml
	$(CC) -c $(ZAM-MINIML)src/alloc.ml
	$(CC) -c $(ZAM-MINIML)src/call.ml
	$(CC) -c $(ZAM-MINIML)src/interp.ml
	$(CC) -c $(ZAM-MINIML)input.ml
	$(CC) -c $(ZAM-MINIML)src/main.ml
	$(CC) -o $(EXE) $(MLVALUES:.ml=.cmo) $(ZAM-MINIML)src/prims.cmo \
	$(ZAM-MINIML)src/domain.cmo $(ZAM-MINIML)src/block.cmo \
	$(ZAM-MINIML)src/data.cmo $(ZAM-MINIML)src/gc.cmo $(ZAM-MINIML)src/alloc.cmo \
	$(ZAM-MINIML)src/call.cmo $(ZAM-MINIML)input.cmo $(ZAM-MINIML)src/interp.cmo  \
	$(ZAM-MINIML)src/main.cmo

clean:
	rm -f $(EXE) $(ZAM-MINIML)*.cm[oi]

clean-all:	clean
	rm -f $(EXE)
