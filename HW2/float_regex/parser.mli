type token =
  | NEWLINE
  | FLOAT of (float)

val expr :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> float
