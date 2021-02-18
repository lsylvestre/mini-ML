type token =
  | EOF
  | POP
  | PUSH
  | FUNCTION
  | RETURN
  | CALL
  | GOTO
  | IFGOTO
  | LABEL
  | DOT
  | CONSTANT
  | STATIC
  | LOCAL
  | ARGUMENT
  | TEMP
  | THIS
  | THAT
  | ADD
  | SUB
  | LAND
  | LOR
  | NOT
  | LT
  | GT
  | EQ
  | EXIT
  | IDENT of (string)
  | FNAME of (string)
  | MIDENT of (string)
  | INT of (int)

val prog :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Bc.prog
