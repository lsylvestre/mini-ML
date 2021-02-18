############################################################################
##                                                                        ##
##         PSTL : OCaml sur la plate-forme Nand2Tetris (2020)             ##
##                                                                        ##
##           Loïc SYLVESTRE              Pablito BELLO                    ##
##           loic.sylvestre@etu.upmc.fr  pablito.bello@etu.upmc.fr        ##
##                                                                        ##
############################################################################

CC=ocamlc
EXE=run

all:
	$(CC) -c parseutils.ml
	ocamllex lexer.mll
	ocamlyacc parser.mly
	$(CC) -c bc.ml
	$(CC) -c kbc.ml
	$(CC) -c collect.ml
	$(CC) -c bc2kbc.ml
	$(CC) -c parser.mli
	$(CC) -c lexer.ml
	$(CC) -c parser.ml	
	$(CC) -c interp.ml
	$(CC) -c main.ml
	
	$(CC) -o $(EXE) parseutils.cmo lexer.cmo parser.cmo bc.cmo kbc.cmo \
	collect.cmo bc2kbc.cmo interp.cmo main.cmo

clean:
	rm -f $(EXE) *.cmo *.cmi run *.vm *.tst *.out 