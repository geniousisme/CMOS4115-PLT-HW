##COMS 4115 Programming Language and Translator - HW1
###Chia-Hao Hsu (ch3141)


##Prob. 1
Write a function that subtracts positive integers represented
as lists of decimal digits. For example,

    subl [2;5;3] [5;7] = [1;9;6]
    subl [1;0;0;0;0;0;0;0;0;0;0;0] [4;2;0;0;0;0;0;0;0;0;0] =
    [0;5;8;0;0;0;0;0;0;0;0;0]
    subl [1] [0];;
    subl [1; 0; 0] [1];;

Your algorithm may assume the first number is larger
than the second. Arbitrary-precision arithmetic packages
use a similar technique but with a much larger radix.

###Ans:
---
####subtract.ml
```ocaml
let subl lst1 lst2 =
    (* list_to_int: transfer int list into a int number first *)
    let list_to_int lst =
         List.fold_left (fun a b -> 10 * a + b) 0 lst
    in
    (* int_to_list: transfer the int number into int list *)
    let rec int_to_list target_length target_int res_list =
        if target_int = 0 && target_length = 0 then 0 :: res_list
        else if target_int < 10 && target_length = 0 then target_int :: res_list
        else if target_int < 10 && target_length > 0 then int_to_list (target_length - 1) 0 (target_int :: res_list)
        else int_to_list (target_length - 1) (target_int / 10) ((target_int mod 10) :: res_list)
    in
    (* subtract two int value first, then transfer the int value into list *)
    int_to_list (List.length lst1 - 1) ((list_to_int lst1) - (list_to_int lst2)) []
;;

let () =
    assert(subl [1] [0] = [1]);
    assert(subl [1; 0; 0; 0] [1] = [0; 9; 9; 9]);
    assert(subl [2; 5; 3] [5; 7] = [1; 9; 6]);
    assert(subl [1;0;0;0;0;0;0;0;0;0;0;0] [4;2;0;0;0;0;0;0;0;0;0] = [0; 5; 8; 0; 0; 0; 0; 0; 0; 0; 0; 0])
;;
```
To test my code, first I run following on my terminal:

    $ocaml
    # let subl lst1 lst2 =
        (* list_to_int: transfer int list into a int number first *)
        let list_to_int lst =
             List.fold_left (fun a b -> 10 * a + b) 0 lst in
        (* int_to_list: transfer the int number into int list *)
        let rec int_to_list target_length target_int res_list =
            if target_int = 0 && target_length = 0 then 0 :: res_list
            else if target_int < 10 && target_length = 0 then target_int :: res_list
            else if target_int < 10 && target_length > 0 then int_to_list (target_length - 1) 0 (target_int :: res_list)
            else int_to_list (target_length - 1) (target_int / 10) ((target_int mod 10) :: res_list) in
        (* subtract two int value first, then transfer the int value into list *)
        int_to_list (List.length lst1 - 1) ((list_to_int lst1) - (list_to_int lst2)) []
    ;;
                            val subl : int list -> int list -> int list = <fun>
    # subl [1] [0];;
    - : int list = [1]
    # subl [1] [0];;
    - : int list = [1]
    # subl [1; 0; 0; 0] [1];;
    - : int list = [0; 9; 9; 9]
    # subl [2; 5; 3] [5; 7];;
    - : int list = [1; 9; 6]
    # subl [1;0;0;0;0;0;0;0;0;0;0;0] [4;2;0;0;0;0;0;0;0;0;0];;
    - : int list = [0; 5; 8; 0; 0; 0; 0; 0; 0; 0; 0; 0]

Then I use assert to run my code directly:

    $ ocaml subtract/subtract.ml
    $

No Exception Error, yeah!

##Prob. 2
Write a word frequency counter starting from the follow- ing ocamllex program (wordcount.mll)
that gathers all the words in a file and prints them.

```ocaml
    { type token = EOF | Word of string }
        rule token = parse
        | eof { EOF }
        | ['a'-'z' 'A'-'Z']+ as word { Word(word) }
        | _ { token lexbuf }
    {
        let lexbuf = Lexing.from_channel stdin in
        let wordlist =
        let rec next l = match token lexbuf with
                           EOF -> l
                         | Word(s) -> next (s :: l)
            in next [] in
        List.iter print_endline wordlist
    }
```

Replace the List.iter line with code that builds a string map of (word, count) pairs,
uses StringMap.fold to con- vert the map to a list of (count, word) pairs,
sorts the pairs using List.sort, and prints them with List.iter.

Sort the list of (count, word) pairs using

```ocaml
  let wordcounts =
      List.sort (fun (c1, _) (c2, _) ->
            Pervasives.compare c2 c1)
      wordcounts in
```
Compiling and running my (20-more-line) solution:

    $ ocamllex wordcount.mll
    4 states, 315 transitions, table size 1284 bytes
    $ ocamlc -o wordcount wordcount.ml
    $ ./wordcount < wordcount.mll
    9 word
    7 map
    7 let
    7 StringMap 6in
    ...

###Ans:
---
####wordcount.mll
```ocaml
{ type token = EOF | Word of string }

rule token = parse
    | eof { EOF }
    | ['a'-'z' 'A'-'Z']+ as word { Word(word) }
    | _ { token lexbuf }


{
    module StringMap = Map.Make(String);;

    let lexbuf = Lexing.from_channel stdin in
    let wordlist =
        let rec next l = match token lexbuf with
                            EOF -> l
                            | Word(s) -> next (s :: l)
        in next []
    in
    (* Build the word map *)
    let word_count_map =
        let rec word_count_helper word_map = function
            | []       -> word_map
            | hd :: tl -> word_count_helper
                          (if not (StringMap.mem hd word_map) then StringMap.add hd 1 word_map
                           else StringMap.add hd ((StringMap.find hd word_map) + 1) word_map) tl
            in  word_count_helper StringMap.empty wordlist
    in
    (* Build the the count word tuple list function for map fold function *)
    let count_word_list_generator word count lst = (count, word) :: lst
    in
    (* Fold the word_count_map to tuple list with count first, word in the second position *)
    let count_word_list =
                        StringMap.fold count_word_list_generator word_count_map []
    in
    (* Sort the count_word_list *)
    let wordcounts =
        List.sort (fun (c1, _) (c2, _) -> Pervasives.compare c2 c1) count_word_list
    in
    (* Make the specifc print function for tuple list *)
    let print_count_word_tuple tuple = match tuple with
        (count, word) -> print_endline (string_of_int(count) ^ " " ^ word)
    in
    (* Print out all results with assigned format *)
    List.iter print_count_word_tuple wordcounts
}
```
####Makefile
```makefile
all:
	ocamllex wordcount.mll
	ocamlc -o wordcount wordcount.ml
	./wordcount < wordcount.mll

clean:
	rm -f wordcount *.cmi *.cmo *.ml
```

I ran following commands to test wordcount program:

    $make
    ocamllex wordcount.mll
    4 states, 315 transitions, table size 1284 bytes
    ocamlc -o wordcount wordcount.ml
    ./wordcount < wordcount.mll
    28 word
    19 count
    11 map
    10 in
    9 let
    8 list
    7 tuple
    7 the
    7 StringMap
    5 hd
    4 with
    4 token
    4 print
    4 function
    4 c
    3 next
    3 lexbuf
    3 l
    3 helper
    3 Word
    3 EOF
    2 wordlist
    2 wordcounts
    2 tl
    2 string
    2 s
    2 rec
    2 of
    2 match
    2 lst
    2 generator
    2 for
    2 fold
    2 add
    2 Make
    2 List
    2 Build
    1 z
    1 type
    1 to
    1 then
    1 stdin
    1 specifc
    1 sort
    1 second
    1 rule
    1 results
    1 position
    1 parse
    1 out
    1 not
    1 module
    1 mem
    1 iter
    1 int
    1 if
    1 fun
    1 from
    1 format
    1 first
    1 find
    1 eof
    1 endline
    1 empty
    1 else
    1 compare
    1 channel
    1 assigned
    1 as
    1 all
    1 a
    1 Z
    1 String
    1 Sort
    1 Print
    1 Pervasives
    1 Map
    1 Lexing
    1 Fold
    1 A

##Prob. 3
Extend the three-slide"calculator"example shown at the end of the Introduction to OCaml slides
(the source is available on the class website) to accept the variables named a through z, 
assignment to those variables, and sequencing using the "," operator. For example,

    a = b = 3, b = b + 3, a * b + 2

should print "20"
Use an array of length 26 initialized to all zeros to store the values of the variables.
You’ll need to add tokens to the parser and scanner for representing assignment, sequencing, and variable names.
The ocamllex rule for the variable names, which converts the letters a–z into a VARIABLE token, is
    | [’a’-’z’] as lit
      { VARIABLE(int_of_char lit - 97) }
The new ast.mli file is
```ocaml
type operator = Add | Sub | Mul | Div
type expr =
    Binop of expr * operator * expr
| Lit of int
  | Seq of expr * expr
  | Asn of int * expr
  | Var of int
```
