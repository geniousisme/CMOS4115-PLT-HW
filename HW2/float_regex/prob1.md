##COMS 4115 Programming Language and Translator - HW2
###Chia-Hao Hsu (ch3141)


##Prob. 1
Write regular expressions in an Ocamllex style for floating point literals 
according to the following (after K&R):

A floating constant consists of an integer part, a decimal point,
a fraction part, an e or E, and an op- tionally signed integer exponent. 
The integer and fraction parts both consist of a sequence of dig- its.
Either the integer part, or the fraction part (not both) may be missing; 
either the decimal point or the e/E and the exponent (not both) may be miss- ing.

Hint: make sure your scanner accepts constants such as 
1. 0.5e-15 .3e+3 .2 1e5 3.5E-4 but not integer con- stants such as 42

###Ans:
---
The following are the regular expression I wrote for this question:
```ocaml
(* digit: represent the digit number *)
let digit = ['0'-'9']

(* exponent: represent the e & E cases *)
let exponent = ['e' 'E']

(* operator: represent the + - symbol cases *)
let operator = ['-' '+']

(* use the expr for fraction part *)
let tail_expr = (exponent(operator)?digit+)

let float_re = (digit+('.'))|(digit*('.')digit+tail_expr?)|((digit)tail_expr*)
```
To test my code, I ran the test with program provided by TA on Piazza:

```ocaml
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
```



    
