############################################################################
##                                                                        ##
##         PSTL : OCaml sur la plate-forme Nand2Tetris (2020)             ##
##                                                                        ##
##           Loïc SYLVESTRE              Pablito BELLO                    ##
##           loic.sylvestre@etu.upmc.fr  pablito.bello@etu.upmc.fr        ##
##                                                                        ##
############################################################################

CC=ocamlc
EXE=compile

all:
	$(CC) -c parseutils.ml
	ocamllex lexer.mll
	ocamlyacc parser.mly
	$(CC) -c ast.ml
	$(CC) -c types.ml
	$(CC) -c typing.ml
	$(CC) -c parser.mli
	$(CC) -c lexer.ml
	$(CC) -c parser.ml
	$(CC) -c bc.ml
	$(CC) -c compile.ml
	$(CC) -c main.ml
	
	$(CC) -o $(EXE) parseutils.cmo types.cmo typing.cmo ast.cmo lexer.cmo \
	 parser.cmo bc.cmo compile.cmo main.cmo

clean:
	rm -f $(EXE) *.cmo *.cmi compile *.vm *.tst *.out 