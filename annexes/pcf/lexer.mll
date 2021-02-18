(**************************************************************************)
(*                                                                        *)
(*         PSTL : OCaml sur la plate-forme Nand2Tetris (2020)             *)
(*                                                                        *)      
(*           Loïc SYLVESTRE              Pablito BELLO                    *)
(*           loic.sylvestre@etu.upmc.fr  pablito.bello@etu.upmc.fr        *)
(*                                                                        *)  
(**************************************************************************)

{
  open Parseutils
  open Parser        (* The type token is defined in parser.mli *)
  exception Eof
}

let ident = ['a'-'z''_'] ['a'-'z''A'-'Z''0'-'9''_']*

rule token = parse
| (['0'-'9']+) as n { INT(int_of_string n) }
| '('               { LPAREN }
| ')'               { RPAREN }
| '\\'              { FUN }
| "."               { DOT }
| "+"               { ADD }
| "-"               { SUB }
| "="               { EQ }
| ";;"              { END }
| ident as lxm      { match lxm with 
	                  | "ifz" -> IFZ
	                  | "then" -> THEN
	                  | "else" -> ELSE
	                  | "let" -> LET
	                  | "letrec" -> LETREC
	                  | "in" -> IN
	                  | _ -> IDENT(lxm) }
| ['\n' ]            { (Lexing.new_line lexbuf) ; (token lexbuf) }
| [' ' '\t']         { token lexbuf }    (* skip blanks *)
| "(*"              { comment lexbuf }  (* Comment until closing *)
| eof               { EOF }
| _  as lxm         { raise (Parse_Exception (Printf.sprintf "Unexpected character: %c"  lxm,  Parseutils.pos())) }


and comment = parse 
| "*)" { token lexbuf }
| _    { comment lexbuf } 
