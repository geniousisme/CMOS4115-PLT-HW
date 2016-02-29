let _ =
  let lexbuf = Lexing.from_channel stdin in
  let result = Parser.expr Scanner.token lexbuf in
  let _ = print_float result in
  print_newline()