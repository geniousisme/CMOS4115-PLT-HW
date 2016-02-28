(*
Extend the three-slide"calculator"example shown at the end of the Introduction to OCaml slides (the source is available on the class website) to accept the variables named a through z, assignment to those variables, and sequencing using the “,” operator. For example,
a = b = 3, b = b + 3, a * b + 2
should print "20"
Use an array of length 26 initialized to all zeros to store the values of the variables. You’ll need to add tokens to the parser and scanner for representing assignment, se- quencing, and variable names.
The ocamllex rule for the variable names, which converts the letters a–z into a VARIABLE token, is
| [’a’-’z’] as lit
  { VARIABLE(int_of_char lit - 97) }
The new ast.mli file is
type operator = Add | Sub | Mul | Div
type expr =
    Binop of expr * operator * expr
| Lit of int
  | Seq of expr * expr
  | Asn of int * expr
  | Var of int
*)
open Ast

let vars = Array.make 26 0
let rec eval = function
    Lit(x) -> x
    | Binop(e1, op, e2) ->
        let v1 = eval e1 and v2 = eval e2 in
            (match op with
                Add -> v1 + v2
                | Sub -> v1 - v2
                | Mul -> v1 * v2
                | Div -> v1 / v2)
    | Asn(v, e) ->
        let res = eval e in
            vars.(v) <- res; res
    | Seq(e1, e2) -> ignore (eval e1); eval e2
    | Var(v) -> vars.(v)

let _ =
    let lexbuf = Lexing.from_channel stdin in
    let expr = Parser.expr Scanner.token lexbuf in
    let result = eval expr in
    print_endline (string_of_int result)
(*

I ran following command to compile my calculator:

    $ make
    ocamllex scanner.mll
    11 states, 271 transitions, table size 1150 bytes
    ocamlyacc parser.mly
    ocamlc -c ast.mli
    ocamlc -c parser.mli
    ocamlc -c scanner.ml
    ocamlc -c parser.ml
    ocamlc -c calc.ml
    ocamlc -o calc parser.cmo scanner.cmo calc.cmo

I ran following command to test my calc execution file

    ./calc
    a = b = 3, b = b + 3, a *b + 2
    20

    ./calc
    2 + 100 - 10
    92

    ./calc
    a = b = c = 4, a = b + c, a / c + b * b
    18

    ./calc
    a = b = 10, c = 100, a + b - c
    -80


*)