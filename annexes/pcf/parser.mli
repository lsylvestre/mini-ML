type token =
  | LET
  | LETREC
  | IFZ
  | IN
  | IF
  | THEN
  | ELSE
  | INT of (int)
  | FUN
  | DOT
  | ADD
  | SUB
  | EQ
  | END
  | LPAREN
  | RPAREN
  | EOF
  | IDENT of (string)

val prog :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.exp
