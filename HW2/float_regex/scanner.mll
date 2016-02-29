{ open Parser }

(* 
	From the cases provided by HW document (i.e. 1. 0.5e-15 .3e+3 .2 1e5 3.5E-4)
	We can know there are following cases for the regular expression:
	1. fraction (ex. 1., 1.2, 123.456)
	2. fraction e|E +|- int ()
	3. decimal_point int e|E +|- int (.12e+2, .4E+10) 
	4. decimal_point int (.12, .1)
	5. fraction e|E int (7.823E5, 1.2e−4)
*)

let digit = ['0'-'9']
let exponent = ['e' 'E']
let operator = ['-' '+']
let tail_expr = (exponent(operator)?digit+)

let float_expr = 
		digit+('.')
		| digit*('.')digit+tail_expr?
		| (digit)tail_expr

rule token = parse
		'\n' 				{ NEWLINE }
		| float_expr as lit { FLOAT(float_of_string lit) }

