(* 
    1. 
    From the cases provided by HW document (i.e. 1. 0.5e-15 .3e+3 .2 1e5 3.5E-4)
    We can know there are following cases for the regular expression:
    1. fraction (ex. 1., 1.2, 123.456)
    2. fraction e|E +|- int (ex. 0.5e-15, )3.5E-4
    3. decimal_point int e|E +|- int (ex. .12e+2, .4E+10) 
    4. decimal_point int (ex. .12, .1)
    5. fraction e|E int (ex. 7.823E5, 1.2e4, 1e5)
*)

{
    open Printf
}

let digit = ['0'-'9']
let exponent = ['e' 'E']
let operator = ['-' '+']
let tail_expr = (exponent(operator)?digit+)

rule floating_pt=parse
    |((digit+('.'))|(digit*('.')digit+tail_expr?)|((digit)tail_expr*)) { print_string "cool\n" }
    
{
    let main () =
    let cin =
      if Array.length Sys.argv > 1
      then open_in Sys.argv.(1)
      else stdin
    in
    let lexbuf = Lexing.from_channel cin in
    floating_pt lexbuf

  let _ = Printexc.print main ()

}
