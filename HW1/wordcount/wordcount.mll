(*
    2.
    Write a word frequency counter starting from the follow- ing ocamllex program (wordcount.mll) that gathers all the words in a file and prints them.

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

    Replace the List.iter line with code that builds a string map of (word, count) pairs, uses StringMap.fold to con- vert the map to a list of (count, word) pairs, sorts the pairs using List.sort, and prints them with List.iter.
    Sort the list of (count, word) pairs using

    let wordcounts =
        List.sort (fun (c1, _) (c2, _) ->
              Pervasives.compare c2 c1)
     wordcounts in

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

*)

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

(*
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
*)
