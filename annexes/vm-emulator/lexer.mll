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
  open Parser

  exception Eof
}

let ident = ['a'-'z''_']['a'-'z''A'-'Z''0'-'9''_']*
let fname = ['A'-'Z']['a'-'z''A'-'Z''0'-'9''_']*['.'][^' ''\t''\n']+
let identM  = ['A'-'Z']['a'-'z''A'-'Z''0'-'9''_']*

rule token = parse
| (['0'-'9']+) as n { INT(int_of_string n) }
| "pop"  { POP }
| "push"  { PUSH }
| "function"  { FUNCTION }
| "return"  { RETURN }
| "call"  { CALL}
| "goto"  { GOTO }
| "if-goto"  { IFGOTO }
| "label"  {  LABEL}
| "constant" {  CONSTANT}
| "static"  { STATIC }
| "local"  { LOCAL }
| "argument"  { ARGUMENT }
| "temp"  { TEMP }
| "this"  { THIS }
| "that"  { THAT}
| "add"  { ADD }
| "sub"  { SUB }
| "land"  { LAND }
| "lor"  { LOR }
| "not"  { NOT }
| "lt"  { LT }
| "gt"  { GT }
| "eq"  { EQ}
| ident as lxm      { match lxm with 
	                  | _ -> IDENT(lxm) }
| identM as lxm      { MIDENT(lxm) }
| fname as lxm       { FNAME(lxm) }
| ['\n' ]            { (Lexing.new_line lexbuf) ; (token lexbuf) }
| [' ' '\t']         { token lexbuf }    (* skip blanks *)
| "//"[^'\n']*       { token lexbuf }  
| eof               { EOF }
| _  as lxm         { raise (Parse_Exception (Printf.sprintf "Unexpected character: %c"  lxm,  Parseutils.pos())) }
